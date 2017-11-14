{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ModuleBuilder where

import Prelude hiding (and, or)

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict

import qualified Data.HashSet as HS

import LLVM.AST hiding (function)
import LLVM.AST.Global
import qualified LLVM.AST.Constant as C

import Util.SnocList
import IRBuilder.Monad

newtype ModuleBuilderT m a = ModuleBuilderT { unModuleBuilderT :: StateT ModuleBuilderState m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadTrans, MonadState ModuleBuilderState)

newtype ModuleBuilderState = ModuleBuilderState
  { builderDefs :: SnocList Definition
  }

emptyModuleBuilder :: ModuleBuilderState
emptyModuleBuilder = ModuleBuilderState
  { builderDefs = mempty
  }

type ModuleBuilder = ModuleBuilderT Identity
type MonadModuleBuilder = MonadState ModuleBuilderState

-- | Evaluate 'ModuleBuilder' to a list of definitions
runModuleBuilder :: ModuleBuilderState -> ModuleBuilder a -> [Definition]
runModuleBuilder s (ModuleBuilderT m) = getSnocList $ builderDefs $ execState m s

-- | Evaluate 'ModuleBuilderT' to a list of definitions
runModuleBuilderT :: Monad m => ModuleBuilderState -> ModuleBuilderT m a -> m [Definition]
runModuleBuilderT s (ModuleBuilderT m) = getSnocList . builderDefs <$> execStateT m s

emitDefn :: MonadModuleBuilder m => Definition -> m ()
emitDefn def = modify $ \s -> s { builderDefs = builderDefs s `snoc` def }

-- | Define and emit a function definition.
function
  :: MonadModuleBuilder m
  => Name  -- ^ Function name
  -> [(Type, Name)]  -- ^ Parameters (non-variadic)
  -> Type  -- ^ Return type
  -> ([Operand] -> IRBuilderT m ())  -- ^ Function body builder
  -> m Operand
function label argtys retty body = do
  let
    params = [LocalReference ty nm | (ty, nm) <- argtys]
    irBuilder = emptyIRBuilder
      { builderUsedNames = HS.fromList [n | (_, Name n) <- argtys]
      }
  blocks <- runIRBuilderT irBuilder $ body params
  let
    def = GlobalDefinition functionDefaults
      { name        = label
      , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
      , returnType  = retty
      , basicBlocks = blocks
      }
    funty = FunctionType retty (fst <$> argtys) False
  emitDefn def
  pure $ ConstantOperand $ C.GlobalReference funty label
