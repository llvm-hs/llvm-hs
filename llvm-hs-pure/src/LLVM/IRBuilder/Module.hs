{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-} -- For MonadState s (IRBuilderT m) instance
{-# LANGUAGE CPP #-}

module LLVM.IRBuilder.Module where

import Prelude hiding (and, or)

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Except
import qualified Control.Monad.Fail as Fail
import Control.Monad.Identity
import Control.Monad.Writer.Lazy as Lazy
import Control.Monad.Writer.Strict as Strict
import Control.Monad.Reader
import Control.Monad.RWS.Lazy as Lazy
import Control.Monad.RWS.Strict as Strict
import qualified Control.Monad.State.Strict as Strict
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
#if !(MIN_VERSION_mtl(2,2,2))
import Control.Monad.Trans.Identity
#endif

import Data.Bifunctor
import Data.ByteString.Short as BS
import Data.Char
import Data.Data
import Data.Foldable
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.String

import GHC.Generics(Generic)

import LLVM.AST hiding (function)
import LLVM.AST.Global
import LLVM.AST.Linkage
import LLVM.AST.Type (ptr)
import qualified LLVM.AST.Constant as C

import LLVM.IRBuilder.Internal.SnocList
import LLVM.IRBuilder.Monad

newtype ModuleBuilderT m a = ModuleBuilderT { unModuleBuilderT :: StateT ModuleBuilderState m a }
  deriving
    ( Functor, Alternative, Applicative, Monad, MonadCont, MonadError e
    , MonadFix, MonadIO, MonadPlus, MonadReader r, MonadTrans, MonadWriter w
    )

instance MonadFail m => MonadFail (ModuleBuilderT m) where
  fail str = ModuleBuilderT (StateT $ \_ -> Fail.fail str)

data ModuleBuilderState = ModuleBuilderState
  { builderDefs :: SnocList Definition
  , builderTypeDefs :: Map Name Type
  }

emptyModuleBuilder :: ModuleBuilderState
emptyModuleBuilder = ModuleBuilderState
  { builderDefs = mempty
  , builderTypeDefs = mempty
  }

type ModuleBuilder = ModuleBuilderT Identity

class Monad m => MonadModuleBuilder m where
  liftModuleState :: State ModuleBuilderState a -> m a

  default liftModuleState
    :: (MonadTrans t, MonadModuleBuilder m1, m ~ t m1)
    => State ModuleBuilderState a
    -> m a
  liftModuleState = lift . liftModuleState

instance Monad m => MonadModuleBuilder (ModuleBuilderT m) where
  liftModuleState (StateT s) = ModuleBuilderT $ StateT $ pure . runIdentity . s



-- | Evaluate 'ModuleBuilder' to a result and a list of definitions
runModuleBuilder :: ModuleBuilderState -> ModuleBuilder a -> (a, [Definition])
runModuleBuilder s m = runIdentity $ runModuleBuilderT s m

-- | Evaluate 'ModuleBuilderT' to a result and a list of definitions
runModuleBuilderT :: Monad m => ModuleBuilderState -> ModuleBuilderT m a -> m (a, [Definition])
runModuleBuilderT s (ModuleBuilderT m)
  = second (getSnocList . builderDefs)
  <$> runStateT m s

-- | Evaluate 'ModuleBuilder' to a list of definitions
execModuleBuilder :: ModuleBuilderState -> ModuleBuilder a -> [Definition]
execModuleBuilder s m = snd $ runModuleBuilder s m

-- | Evaluate 'ModuleBuilderT' to a list of definitions
execModuleBuilderT :: Monad m => ModuleBuilderState -> ModuleBuilderT m a -> m [Definition]
execModuleBuilderT s m = snd <$> runModuleBuilderT s m

emitDefn :: MonadModuleBuilder m => Definition -> m ()
emitDefn def = liftModuleState $ modify $ \s -> s { builderDefs = builderDefs s `snoc` def }

-- | A parameter name suggestion
data ParameterName
  = NoParameterName
  | ParameterName ShortByteString
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | Using 'fromString` on non-ASCII strings will throw an error.
instance IsString ParameterName where
  fromString s
    | all isAscii s = ParameterName (fromString s)
    | otherwise =
      error ("Only ASCII strings are automatically converted to LLVM parameter names. "
      <> "Other strings need to be encoded to a `ShortByteString` using an arbitrary encoding.")

-- | Define and emit a (non-variadic) function definition
function
  :: MonadModuleBuilder m
  => Name  -- ^ Function name
  -> [(Type, ParameterName)]  -- ^ Parameter types and name suggestions
  -> Type  -- ^ Return type
  -> ([Operand] -> IRBuilderT m ())  -- ^ Function body builder
  -> m Operand
function label argtys retty body = do
  let tys = fst <$> argtys
  (paramNames, blocks) <- runIRBuilderT emptyIRBuilder $ do
    paramNames <- forM argtys $ \(_, paramName) -> case paramName of
      NoParameterName -> fresh
      ParameterName p -> fresh `named` p
    body $ zipWith LocalReference tys paramNames
    return paramNames
  let
    def = GlobalDefinition functionDefaults
      { name        = label
      , parameters  = (zipWith (\ty nm -> Parameter ty nm []) tys paramNames, False)
      , returnType  = retty
      , basicBlocks = blocks
      }
    funty = ptr $ FunctionType retty (fst <$> argtys) False
  emitDefn def
  pure $ ConstantOperand $ C.GlobalReference funty label

-- | An external function definition
extern
  :: MonadModuleBuilder m
  => Name   -- ^ Definition name
  -> [Type] -- ^ Parameter types
  -> Type   -- ^ Type
  -> m Operand
extern nm argtys retty = do
  emitDefn $ GlobalDefinition functionDefaults
    { name        = nm
    , linkage     = External
    , parameters  = ([Parameter ty (mkName "") [] | ty <- argtys], False)
    , returnType  = retty
    }
  let funty = ptr $ FunctionType retty argtys False
  pure $ ConstantOperand $ C.GlobalReference funty nm

-- | An external variadic argument function definition
externVarArgs
  :: MonadModuleBuilder m
  => Name   -- ^ Definition name
  -> [Type] -- ^ Parameter types
  -> Type   -- ^ Type
  -> m Operand
externVarArgs nm argtys retty = do
  emitDefn $ GlobalDefinition functionDefaults
    { name        = nm
    , linkage     = External
    , parameters  = ([Parameter ty (mkName "") [] | ty <- argtys], True)
    , returnType  = retty
    }
  let funty = ptr $ FunctionType retty argtys True
  pure $ ConstantOperand $ C.GlobalReference funty nm

-- | A global variable definition
global
  :: MonadModuleBuilder m
  => Name       -- ^ Variable name
  -> Type       -- ^ Type
  -> C.Constant -- ^ Initializer
  -> m Operand
global nm ty initVal = do
  emitDefn $ GlobalDefinition globalVariableDefaults
    { name                  = nm
    , LLVM.AST.Global.type' = ty
    , linkage               = External
    , initializer           = Just initVal
    }
  pure $ ConstantOperand $ C.GlobalReference (ptr ty) nm

-- | A named type definition
typedef
  :: MonadModuleBuilder m
  => Name
  -> Maybe Type
  -> m Type
typedef nm ty = do
  emitDefn $ TypeDefinition nm ty
  for_ ty $ \ty' ->
    liftModuleState (modify (\s -> s { builderTypeDefs = Map.insert nm ty' (builderTypeDefs s) }))
  pure (NamedTypeReference nm)

-- | Convenience function for module construction
buildModule :: ShortByteString -> ModuleBuilder a -> Module
buildModule nm = mkModule . execModuleBuilder emptyModuleBuilder
  where
    mkModule ds = defaultModule { moduleName = nm, moduleDefinitions = ds }

-- | Convenience function for module construction (transformer version)
buildModuleT :: Monad m => ShortByteString -> ModuleBuilderT m a -> m Module
buildModuleT nm = fmap mkModule . execModuleBuilderT emptyModuleBuilder
  where
    mkModule ds = defaultModule { moduleName = nm, moduleDefinitions = ds }

-------------------------------------------------------------------------------
-- mtl instances
-------------------------------------------------------------------------------

instance MonadState s m => MonadState s (ModuleBuilderT m) where
  state = lift . state

instance MonadModuleBuilder m => MonadModuleBuilder (ContT r m)
instance MonadModuleBuilder m => MonadModuleBuilder (ExceptT e m)
instance MonadModuleBuilder m => MonadModuleBuilder (IdentityT m)
instance MonadModuleBuilder m => MonadModuleBuilder (MaybeT m)
instance MonadModuleBuilder m => MonadModuleBuilder (ReaderT r m)
instance (MonadModuleBuilder m, Monoid w) => MonadModuleBuilder (Strict.RWST r w s m)
instance (MonadModuleBuilder m, Monoid w) => MonadModuleBuilder (Lazy.RWST r w s m)
instance MonadModuleBuilder m => MonadModuleBuilder (StateT s m)
instance MonadModuleBuilder m => MonadModuleBuilder (Strict.StateT s m)
instance (Monoid w, MonadModuleBuilder m) => MonadModuleBuilder (Strict.WriterT w m)

-- Not an mtl instance, but necessary in order for @globalStringPtr@ to compile
instance MonadModuleBuilder m => MonadModuleBuilder (IRBuilderT m)
