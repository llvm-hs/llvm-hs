{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IRBuilder (
  -- ** Builder Monad
  IRBuilder,
  IRBuilderState,
  runIRBuilder,
  emptyIRBuilder,

  -- ** Module
  function,
  block,

  -- ** Instructions
  fadd,
  fmul,
  add,
  mul,
  sub,
  phi,
  ret,
  retVoid,

  -- ** Low-level
  genBlock,
  emitInstr,
  emitTerm,
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
import LLVM.AST as AST hiding (function)
import LLVM.AST.Type as AST
import LLVM.AST.Name
import LLVM.AST.Global
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C

-- | This provides a uniform API for creating instructions and inserting them
-- into a basic block: either at the end of a BasicBlock, or at a specific
-- location in a block.
-- Uses an indexed monad to handle local vs module level scope.
newtype IRBuilder s a = IRBuilder (State IRBuilderState a)
  deriving (Functor, Applicative, Monad, MonadFix, MonadState IRBuilderState)

type PartialBlock = ([Named Instruction], Maybe (Named Terminator))

-- | A block reference (lazy in second parameter to allow MonadFix)
type BlockRef = (Name, PartialBlock)

-- Index types
data Toplevel -- Module-level
data Function -- Function-level
data Block    -- Block-level

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

-- | Lift function to toplevel
runLocal :: IRBuilder Function a -> IRBuilder Toplevel a
runLocal = coerce

-- | Lift block to function
runBlock :: IRBuilder Block a -> IRBuilder Function a
runBlock = coerce

-- | Add definition to current module.
addDefn :: Definition -> IRBuilder g ()
addDefn d = do
  defs <- gets (moduleDefinitions . builderModule)
  modify $ \s -> s { builderModule = (builderModule s) { moduleDefinitions = defs ++ [d] } }

block :: Name -> IRBuilder Block r -> IRBuilder Function BlockRef
block nm m = do
  start <- gets builderBlock
  result <- runBlock m
  resultState <- gets builderBlock
  modify $ \s -> s { builderBlock = start }
  pure (nm, resultState)

-- | Generate fresh name
fresh :: IRBuilder Block Name
fresh = do
  n <- gets builderSupply
  modify $ \s -> s { builderSupply = 1 + n }
  pure (UnName n)

-- | Reset name supply
resetFresh :: IRBuilder Toplevel ()
resetFresh = modify $ \s -> s { builderSupply = 0 }

-- | Emit instruction
emitInstr :: Type -> Type -> Instruction -> IRBuilder Block Operand
emitInstr ty retty instr = do
  (instrs, term) <- gets builderBlock
  nm <- fresh
  modify $ \s -> s { builderBlock = ((instrs ++ [nm := instr]), term) }
  pure (localRef retty nm)

-- | Emit terminator
emitTerm :: Terminator -> IRBuilder Block ()
emitTerm term = do
  (instrs, _) <- gets builderBlock
  modify $ \s -> s { builderBlock = (instrs, Just (Do term)) }
  pure ()

-- | Emit function
function
  :: Name                            -- ^ Function name
  -> [(Type, Name)]                  -- ^ Parameters (non-variadic)
  -> Type                            -- ^ Return type
  -> IRBuilder Function [BasicBlock] -- ^ Block generator
  -> IRBuilder Toplevel ()
function label argtys retty blockm = do
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
fadd :: Operand -> Operand -> IRBuilder Block Operand
fadd a b = emitInstr (typeOf a) (typeOf a) $ FAdd NoFastMathFlags a b []

-- Arithmetic and Constants
fmul :: Operand -> Operand -> IRBuilder Block Operand
fmul a b = emitInstr (typeOf a) (typeOf a) $ FMul NoFastMathFlags a b []

-- Arithmetic and Constants
add :: Operand -> Operand -> IRBuilder Block Operand
add a b = emitInstr (typeOf a) (typeOf a) $ Add False False a b []

-- Arithmetic and Constants
mul :: Operand -> Operand -> IRBuilder Block Operand
mul a b = emitInstr (typeOf a) (typeOf a) $ Mul False False a b []

-- Arithmetic and Constants
sub :: Operand -> Operand -> IRBuilder Block Operand
sub a b = emitInstr (typeOf a) (typeOf a) $ Mul False False a b []

-- | Branch
br :: (Name, PartialBlock) -> IRBuilder Block ()
br ~(val, _) = emitTerm (Br val [])

-- | Phi
phi :: [(Operand, BlockRef)] -> IRBuilder Block Operand
phi [] = emitInstr AST.void AST.void $ Phi AST.void [] []
phi incoming@(i:is) = emitInstr AST.void ty $ Phi ty vals []
  where
    ty = typeOf (fst i) -- result type
    vals = [(op, nm) | (op, (nm, _)) <- incoming] -- XXX: slightly ugly

-- | RetVoid
retVoid :: IRBuilder Block ()
retVoid = emitTerm (Ret Nothing [])

-- | Ret
ret :: Operand -> IRBuilder Block ()
ret val = emitTerm (Ret (Just val) [])

-- | Constant
cons :: C.Constant -> Operand
cons = ConstantOperand

-------------------------------------------------------------------------------
-- Testing
-------------------------------------------------------------------------------

c1 :: Operand
c1 = cons $ C.Float (F.Double 10)

c2 :: Operand
c2 = cons $ C.Int 32 10


example :: IO ()
example = T.putStrLn $ ppllvm $ runIRBuilder emptyIRBuilder $ do

  function "foo" [] double $ mdo

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
      l <- phi [(c1, blk1), (c1, blk2) ]
      a <- fadd c1 c1
      b <- fadd a a
      retVoid

    pure [genBlock blk1, genBlock blk2, genBlock blk3]


  function "bar" [] double $ mdo

    blk3 <- block "b3" $ do
      a <- fadd c1 c1
      b <- fadd a a
      retVoid

    pure [genBlock blk3]
