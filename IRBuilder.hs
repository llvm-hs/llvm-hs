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
  BlockRef,
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

import Data.Word
import Data.Coerce
import Data.Map as Map
import Data.Text.Lazy.IO as T
import Data.ByteString.Short as BS

import LLVM.Typed
import LLVM.Pretty
import LLVM.AST hiding (function)
import LLVM.AST.Type as AST
import LLVM.AST.Name
import LLVM.AST.Global
import LLVM.AST.ParameterAttribute
import qualified LLVM.AST as AST
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.FloatingPointPredicate as FP

-- | This provides a uniform API for creating instructions and inserting them
-- into a basic block: either at the end of a BasicBlock, or at a specific
-- location in a block.
-- Uses an indexed monad to handle local vs module level scope.
newtype IRBuilder s a = IRBuilder (State IRBuilderState a)
  deriving (Functor, Applicative, Monad, MonadFix, MonadState IRBuilderState)

-- | A partially constructed block as a sequence of instructions
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
emitInstr
  :: Type -- ^ Return type
  -> Instruction
  -> IRBuilder Block Operand
emitInstr retty instr = do
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


-------------------------------------------------------------------------------
-- Toplevel
-------------------------------------------------------------------------------

-- | Emit block
block
  :: Name                         -- ^ Block name
  -> IRBuilder Block r            -- ^ Basic block generation
  -> IRBuilder Function BlockRef
block nm m = do
  start <- gets builderBlock
  result <- runBlock m
  resultState <- gets builderBlock
  modify $ \s -> s { builderBlock = start }
  pure (nm, resultState)

-- | Emit function
function
  :: Name                            -- ^ Function name
  -> [(Type, Name)]                  -- ^ Parameters (non-variadic)
  -> Type                            -- ^ Return type
  -> IRBuilder Function [BasicBlock] -- ^ Function generation
  -> IRBuilder Toplevel Operand
function label argtys retty blockm = do
  blocks <- runLocal blockm
  let
    def = GlobalDefinition $ functionDefaults {
      name        = label
    , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
    , returnType  = retty
    , basicBlocks = blocks
    }
    funty = FunctionType retty (fst <$> argtys) False
  resetFresh
  addDefn def
  pure $ ConstantOperand $ globalRef funty label

genBlock :: BlockRef -> BasicBlock
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
-- Instructions
-------------------------------------------------------------------------------

fadd :: Operand -> Operand -> IRBuilder Block Operand
fadd a b = emitInstr (typeOf a) $ FAdd NoFastMathFlags a b []

fmul :: Operand -> Operand -> IRBuilder Block Operand
fmul a b = emitInstr (typeOf a) $ FMul NoFastMathFlags a b []

fsub :: Operand -> Operand -> IRBuilder Block Operand
fsub a b = emitInstr (typeOf a) $ FSub NoFastMathFlags a b []

fdiv :: Operand -> Operand -> IRBuilder Block Operand
fdiv a b = emitInstr (typeOf a) $ FDiv NoFastMathFlags a b []

frem :: Operand -> Operand -> IRBuilder Block Operand
frem a b = emitInstr (typeOf a) $ FRem NoFastMathFlags a b []

add :: Operand -> Operand -> IRBuilder Block Operand
add a b = emitInstr (typeOf a) $ Add False False a b []

mul :: Operand -> Operand -> IRBuilder Block Operand
mul a b = emitInstr (typeOf a) $ Mul False False a b []

sub :: Operand -> Operand -> IRBuilder Block Operand
sub a b = emitInstr (typeOf a) $ Sub False False a b []

udiv :: Operand -> Operand -> IRBuilder Block Operand
udiv a b = emitInstr (typeOf a) $ UDiv True a b []

sdiv :: Operand -> Operand -> IRBuilder Block Operand
sdiv a b = emitInstr (typeOf a) $ SDiv True a b []

urem :: Operand -> Operand -> IRBuilder Block Operand
urem a b = emitInstr (typeOf a) $ URem a b []

shl :: Operand -> Operand -> IRBuilder Block Operand
shl a b = emitInstr (typeOf a) $ Shl False False a b []

lshr :: Operand -> Operand -> IRBuilder Block Operand
lshr a b = emitInstr (typeOf a) $ LShr True a b []

ashr :: Operand -> Operand -> IRBuilder Block Operand
ashr a b = emitInstr (typeOf a) $ AShr True a b []

and :: Operand -> Operand -> IRBuilder Block Operand
and a b = emitInstr (typeOf a) $ And a b []

or :: Operand -> Operand -> IRBuilder Block Operand
or a b = emitInstr (typeOf a) $ Or a b []

xor :: Operand -> Operand -> IRBuilder Block Operand
xor a b = emitInstr (typeOf a) $ Xor a b []

alloca :: Type -> Maybe Operand -> Word32 -> IRBuilder Block Operand
alloca ty count align = emitInstr (ptr ty) $ Alloca ty count align []

load :: Operand -> Word32 -> IRBuilder Block Operand
load a align = emitInstr (typeOf a) $ Load False a Nothing align []

store :: Operand -> Word32 -> Operand -> IRBuilder Block Operand
store addr align val = emitInstr (typeOf val) $ Store False addr val Nothing align []

gep :: IRBuilder Block Operand
gep = undefined

trunc :: Operand -> Type -> IRBuilder Block Operand
trunc a to = emitInstr to $ Trunc a to []

zext :: Operand -> Type -> IRBuilder Block Operand
zext a to = emitInstr to $ ZExt a to []

sext :: Operand -> Type -> IRBuilder Block Operand
sext a to = emitInstr to $ SExt a to []

fptoui :: Operand -> Type -> IRBuilder Block Operand
fptoui a to = emitInstr to $ FPToUI a to []

fptosi :: Operand -> Type -> IRBuilder Block Operand
fptosi a to = emitInstr to $ FPToSI a to []

uitofp :: Operand -> Type -> IRBuilder Block Operand
uitofp a to = emitInstr to $ UIToFP a to []

sitofp :: Operand -> Type -> IRBuilder Block Operand
sitofp a to = emitInstr to $ SIToFP a to []

ptrtoint :: Operand -> Type -> IRBuilder Block Operand
ptrtoint a to = emitInstr to $ PtrToInt a to []

inttoptr :: Operand -> Type -> IRBuilder Block Operand
inttoptr a to = emitInstr to $ IntToPtr a to []

bitcast :: Operand -> Type -> IRBuilder Block Operand
bitcast a to = emitInstr to $ BitCast a to []

icmp :: IP.IntegerPredicate -> Operand -> Operand -> IRBuilder Block Operand
icmp pred a b = emitInstr i1 $ ICmp pred a b []

fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> IRBuilder Block Operand
fcmp pred a b = emitInstr i1 $ FCmp pred a b []

-- | Unconditional Branch
br :: (Name, PartialBlock) -> IRBuilder Block ()
br ~(val, _) = emitTerm (Br val [])

-- | Phi
phi :: [(Operand, BlockRef)] -> IRBuilder Block Operand
phi [] = emitInstr AST.void $ Phi AST.void [] []
phi incoming@(i:is) = emitInstr ty $ Phi ty vals []
  where
    ty = typeOf (fst i) -- result type
    vals = [(op, nm) | (op, (nm, _)) <- incoming] -- XXX: slightly ugly

-- | RetVoid
retVoid :: IRBuilder Block ()
retVoid = emitTerm (Ret Nothing [])

call :: Operand -> [(Operand, [ParameterAttribute])] -> IRBuilder Block Operand
call fun args = do
  let retty = case typeOf fun of
        FunctionType r _ _ -> r
        _ -> VoidType -- XXX: or error?
  emitInstr retty Call {
    AST.tailCallKind = Nothing
  , AST.callingConvention = CC.C
  , AST.returnAttributes = []
  , AST.function = Right fun
  , AST.arguments = args
  , AST.functionAttributes = []
  , AST.metadata = []
  }

-- | Ret
ret :: Operand -> IRBuilder Block ()
ret val = emitTerm (Ret (Just val) [])

switch :: IRBuilder Block ()
switch = undefined

select :: IRBuilder Block ()
select = undefined

condBr :: IRBuilder Block ()
condBr = undefined

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
example = T.putStrLn $ ppllvm $ runIRBuilder emptyIRBuilder $ mdo

  foo <- function "foo" [] double $ mdo

    blk1 <- block "b1" $ do
      a <- fadd c1 c1
      b <- fadd a a
      c <- add c2 c2
      br blk2

    blk2 <- block "b2" $ do
      a <- fadd c1 c1
      b <- fadd a a
      c <- call foo []
      br blk3

    blk3 <- block "b3" $ do
      l <- phi [(c1, blk1), (c1, blk2), (c1, blk3)]
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
