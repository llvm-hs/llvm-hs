{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IRBuilder (
  IRBuilder,
  IRBuilderState,
  runIRBuilder,
  emptyIRBuilder,
) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Coerce
import Data.Map as Map
import Data.Text.Lazy.IO as T
import Data.ByteString.Short as BS

import LLVM.Typed
import LLVM.Pretty
import LLVM.AST as AST
import LLVM.AST.Name
import LLVM.AST.Type
import LLVM.AST.Global
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C

-- | This provides a uniform API for creating instructions and inserting them
-- into a basic block: either at the end of a BasicBlock, or at a specific
-- location in a block.
--
-- Uses an indexed monad to handle local vs module level scope.
newtype IRBuilder s a = IRBuilder (State IRBuilderState a)
  deriving (Functor, Applicative, Monad, MonadFix, MonadState IRBuilderState)

type PartialBlock = ([Named Instruction], Maybe (Named Terminator))

-- | Index types
data Toplevel
data Function

-- | Builder monad state
data IRBuilderState = IRBuilderState
  { builderModule :: AST.Module
  , builderSupply :: Word
  , builderBlock  :: PartialBlock
  , builderFunc   :: Maybe Definition
  }

emptyIRBuilder :: IRBuilderState
emptyIRBuilder = IRBuilderState
  { builderModule = emptyModule ""
  , builderSupply = 0
  , builderBlock  = ([], Nothing)
  , builderFunc   = Nothing
  }

-- | Evaluate IRBuilder to a
runIRBuilder :: IRBuilderState -> IRBuilder s a -> AST.Module
runIRBuilder mod (IRBuilder m) = builderModule (execState m mod)

emptyModule :: ShortByteString -> AST.Module
emptyModule label = defaultModule { moduleName = label }

-- | Run a basic block builder action
runLocal :: IRBuilder Function a -> IRBuilder Toplevel a
runLocal = coerce

-- | Add definition to current module.
addDefn :: Definition -> IRBuilder g ()
addDefn d = do
  defs <- gets (moduleDefinitions . builderModule)
  modify $ \s -> s { builderModule = (builderModule s) { moduleDefinitions = defs ++ [d] } }

block :: Name -> IRBuilder a r -> IRBuilder a (Name, PartialBlock)
block nm m = do
  start <- gets builderBlock
  result <- m
  resultState <- gets builderBlock
  modify $ \s -> s { builderBlock = start }
  pure (nm, resultState)

fresh :: IRBuilder Function Name
fresh = do
  n <- gets builderSupply
  modify $ \s -> s { builderSupply = 1 + n }
  pure (UnName n)

resetFresh :: IRBuilder Toplevel ()
resetFresh = modify $ \s -> s { builderSupply = 0 }

emitInstr :: Type -> Type -> Instruction -> IRBuilder Function Operand
emitInstr ty retty instr = do
  (instrs, term) <- gets builderBlock
  nm <- fresh
  modify $ \s -> s { builderBlock = ((instrs ++ [nm := instr]), term) }
  pure (localRef retty nm)

emitFunction
  :: Name                            -- ^ Function name
  -> [(Type, Name)]                  -- ^ Parameters (non-variadic)
  -> Type                            -- ^ Return type
  -> IRBuilder Function [BasicBlock] -- ^ Block generator
  -> IRBuilder Toplevel ()
emitFunction label argtys retty blockm = do
  blocks <- runLocal blockm
  let
    def = GlobalDefinition $ functionDefaults {
      name        = label
    , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
    , returnType  = retty
    , basicBlocks = blocks
    }
  resetFresh
  addDefn def

genBlock :: (Name, PartialBlock) -> BasicBlock
genBlock (blockLabel, (instrs, term)) =
  case term of
    Nothing   -> BasicBlock blockLabel instrs (Do (Ret Nothing []))
    Just term -> BasicBlock blockLabel instrs term

emitTerm :: Terminator -> IRBuilder Function ()
emitTerm term = do
  (instrs, _) <- gets builderBlock
  modify $ \s -> s { builderBlock = (instrs, Just (Do term)) }
  pure ()

-- Local reference
localRef ::  Type -> Name -> Operand
localRef = LocalReference

-- | Global reference
globalRef :: Type -> Name -> C.Constant
globalRef = C.GlobalReference

-------------------------------------------------------------------------------
-- Insturctions
-------------------------------------------------------------------------------

-- Arithmetic and Constants
fadd :: Operand -> Operand -> IRBuilder Function Operand
fadd a b = emitInstr (typeOf a) (typeOf a) $ FAdd NoFastMathFlags a b []

-- Arithmetic and Constants
add :: Operand -> Operand -> IRBuilder Function Operand
add a b = emitInstr (typeOf a) (typeOf a) $ Add False False a b []

-- Control Flow
br :: (Name, PartialBlock) -> IRBuilder Function ()
br ~(val, _) = emitTerm (Br val [])

-- Control Flow
retVoid :: IRBuilder Function ()
retVoid = emitTerm (Ret Nothing [])

cons :: C.Constant -> Operand
cons = ConstantOperand

c1 :: Operand
c1 = cons $ C.Float (F.Double 10)

c2 :: Operand
c2 = cons $ C.Int 32 10

example :: IO ()
example = T.putStrLn $ ppllvm $ runIRBuilder emptyIRBuilder $ do

  emitFunction "foo" [] double $ mdo

    blk1 <- block "b1" $ do
      a <- fadd c1 c1
      b <- fadd a a
      c <- add c2 c2
      br blk2

    blk2 <- block "b2" $ do
      a <- fadd c1 c1
      b <- fadd a a
      br blk3

    blk3 <- block "b3" $ do
      a <- fadd c1 c1
      b <- fadd a a
      retVoid

    pure [genBlock blk1, genBlock blk2, genBlock blk3]


  emitFunction "bar" [] double $ mdo

    blk3 <- block "b3" $ do
      a <- fadd c1 c1
      b <- fadd a a
      retVoid

    pure [genBlock blk3]
