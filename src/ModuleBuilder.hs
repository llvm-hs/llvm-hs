{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-} -- For MonadState s (IRBuilderT m) instance

module ModuleBuilder where

import Prelude hiding (and, or)

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Fail
import Control.Monad.Identity
import Control.Monad.Writer.Lazy as Lazy
import Control.Monad.Writer.Strict as Strict
import Control.Monad.Reader
import Control.Monad.RWS.Lazy as Lazy
import Control.Monad.RWS.Strict as Strict
import qualified Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict
import Control.Monad.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Identity

import Data.Bifunctor
import Data.ByteString.Short as BS

import LLVM.AST hiding (function)
import LLVM.AST.Global
import LLVM.AST.Linkage
import qualified LLVM.AST.Constant as C

import Util.SnocList
import IRBuilder.Monad

newtype ModuleBuilderT m a = ModuleBuilderT { unModuleBuilderT :: StateT ModuleBuilderState m a }
  deriving
    ( Functor, Alternative, Applicative, Monad, MonadCont, MonadError e, MonadFail
    , MonadFix, MonadIO, MonadPlus, MonadReader r, MonadTrans, MonadWriter w
    )

newtype ModuleBuilderState = ModuleBuilderState
  { builderDefs :: SnocList Definition
  }

emptyModuleBuilder :: ModuleBuilderState
emptyModuleBuilder = ModuleBuilderState
  { builderDefs = mempty
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

-- | Define and emit a (non-variadic) function definition
function
  :: MonadModuleBuilder m
  => Name  -- ^ Function name
  -> [(Type, Maybe ShortByteString)]  -- ^ Parameter types and name suggestions
  -> Type  -- ^ Return type
  -> ([Operand] -> IRBuilderT m ())  -- ^ Function body builder
  -> m Operand
function label argtys retty body = do
  let tys = fst <$> argtys
  (paramNames, blocks) <- runIRBuilderT emptyIRBuilder $ do
    paramNames <- forM argtys $ \(_, mname) ->
      maybe fresh (fresh `named`) mname
    body $ zipWith LocalReference tys paramNames
    return paramNames
  let
    def = GlobalDefinition functionDefaults
      { name        = label
      , parameters  = (zipWith (\ty nm -> Parameter ty nm []) tys paramNames, False)
      , returnType  = retty
      , basicBlocks = blocks
      }
    funty = FunctionType retty (fst <$> argtys) False
  emitDefn def
  pure $ ConstantOperand $ C.GlobalReference funty label

-- | An external function definition
extern
  :: MonadModuleBuilder m
  => Name   -- ^ Definition name
  -> [Type] -- ^ Parametere types
  -> Type   -- ^ Type
  -> m Operand
extern name argtys retty = do
  emitDefn $ GlobalDefinition functionDefaults
    { name        = name
    , linkage     = External
    , parameters  = ([Parameter ty (mkName "") [] | ty <- argtys], False)
    , returnType  = retty
    }
  let funty = FunctionType retty argtys False
  pure $ ConstantOperand $ C.GlobalReference funty name

-- | A named type definition
typedef
  :: MonadModuleBuilder m
  => Name
  -> Maybe Type
  -> m ()
typedef nm ty = do
  emitDefn $ TypeDefinition nm ty
  pure ()

-- | Convenience function for module construction
buildModule :: ShortByteString -> [Definition] -> ModuleBuilder a -> Module
buildModule name ds = mkModule . execModuleBuilder emptyModuleBuilder
  where
    mkModule ds = defaultModule { moduleName = name, moduleDefinitions = ds }

-------------------------------------------------------------------------------
-- mtl instances
-------------------------------------------------------------------------------

instance MonadState s m => MonadState s (ModuleBuilderT m) where
  state = lift . state

instance MonadModuleBuilder m => MonadModuleBuilder (ContT r m)
instance MonadModuleBuilder m => MonadModuleBuilder (ExceptT e m)
instance MonadModuleBuilder m => MonadModuleBuilder (IdentityT m)
instance MonadModuleBuilder m => MonadModuleBuilder (ListT m)
instance MonadModuleBuilder m => MonadModuleBuilder (MaybeT m)
instance MonadModuleBuilder m => MonadModuleBuilder (ReaderT r m)
instance (MonadModuleBuilder m, Monoid w) => MonadModuleBuilder (Strict.RWST r w s m)
instance (MonadModuleBuilder m, Monoid w) => MonadModuleBuilder (Lazy.RWST r w s m)
instance MonadModuleBuilder m => MonadModuleBuilder (StateT s m)
instance MonadModuleBuilder m => MonadModuleBuilder (Lazy.StateT s m)
instance (Monoid w, MonadModuleBuilder m) => MonadModuleBuilder (Strict.WriterT w m)
instance (Monoid w, MonadModuleBuilder m) => MonadModuleBuilder (Lazy.WriterT w m)
