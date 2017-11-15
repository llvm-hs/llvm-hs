{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IRBuilder (
  -- ** Builder Monad
  IRBuilder,
  IRBuilderState,
  runIRBuilder,
  emptyIRBuilder,

  -- ** Function
  block,

  -- ** Instructions
  fadd,
  fmul,
  fsub,
  fdiv,
  frem,
  add,
  mul,
  sub,
  udiv,
  sdiv,
  urem,
  shl,
  lshr,
  ashr,
  and,
  or,
  xor,
  alloca,
  load,
  store,
  gep,
  trunc,
  zext,
  sext,
  fptoui,
  fptosi,
  uitofp,
  sitofp,
  ptrtoint,
  inttoptr,
  bitcast,
  icmp,
  fcmp,
  br,
  call,
  switch,
  select,
  condBr,
  phi,
  ret,
  retVoid,

  -- ** Low-level
  emitInstr,
  emitTerm,
) where

import Prelude hiding (and, or)

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Word
import Data.Monoid
import Data.String
import Data.Text.Lazy.IO as T
import Data.ByteString.Short as BS
import Data.HashSet(HashSet)
import qualified Data.HashSet as HS

import LLVM.Typed
import LLVM.Pretty
import LLVM.AST hiding (function)
import LLVM.AST.Type as AST
import LLVM.AST.Global
import LLVM.AST.ParameterAttribute
import qualified LLVM.AST as AST
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.FloatingPointPredicate as FP

newtype SnocList a = SnocList { unSnocList :: Dual [a] }
  deriving (Eq, Show, Monoid)

snoc :: SnocList a -> a -> SnocList a
snoc (SnocList (Dual xs)) x = SnocList $ Dual $ x : xs

getSnocList :: SnocList a -> [a]
getSnocList = reverse . getDual . unSnocList

-- | This provides a uniform API for creating instructions and inserting them
-- into a basic block: either at the end of a BasicBlock, or at a specific
-- location in a block.
newtype IRBuilderT m a = IRBuilderT { unIRBuilderT :: StateT IRBuilderState m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadTrans, MonadState IRBuilderState)

type IRBuilder = IRBuilderT Identity
type MonadIRBuilder = MonadState IRBuilderState

-- | A partially constructed block as a sequence of instructions
data PartialBlock = PartialBlock
  { partialBlockName :: !Name
  , partialBlockInstrs :: SnocList (Named Instruction)
  , partialBlockTerm :: Maybe (Named Terminator)
  }

emptyPartialBlock :: Name -> PartialBlock
emptyPartialBlock nm = PartialBlock nm mempty Nothing

-- | Builder monad state
data IRBuilderState = IRBuilderState
  { builderSupply :: !Word
  , builderUsedNames :: !(HashSet ShortByteString)
  , builderNameSuggestion :: Maybe ShortByteString
  , builderBlocks :: SnocList BasicBlock
  , builderBlock :: !PartialBlock
  }

emptyIRBuilder :: IRBuilderState
emptyIRBuilder = IRBuilderState
  { builderSupply = 1
  , builderUsedNames = mempty
  , builderNameSuggestion = Nothing
  , builderBlocks = mempty
  , builderBlock = emptyPartialBlock $ UnName 0
  }

-- | Evaluate IRBuilder to a list of basic blocks
runIRBuilder :: IRBuilderState -> IRBuilder a -> [BasicBlock]
runIRBuilder s m = getSnocList $ builderBlocks $ execState (unIRBuilderT $ m >> block) s

-- | Evaluate IRBuilderT to a list of basic blocks
runIRBuilderT :: Monad m => IRBuilderState -> IRBuilderT m a -> m [BasicBlock]
runIRBuilderT s m = getSnocList . builderBlocks <$> execStateT (unIRBuilderT $ m >> block) s

-------------------------------------------------------------------------------
-- State Manipulation
-------------------------------------------------------------------------------

modifyBlock
  :: MonadIRBuilder m
  => (PartialBlock -> PartialBlock)
  -> m ()
modifyBlock f = modify $ \s -> s { builderBlock = f $ builderBlock s }

-- | Generate fresh name
fresh :: MonadIRBuilder m => m Name
fresh = do
  msuggestion <- gets builderNameSuggestion
  case msuggestion of
    Nothing -> do
      n <- gets builderSupply
      modify $ \s -> s { builderSupply = 1 + n }
      pure (UnName n)
    Just suggestion -> do
      usedNames <- gets builderUsedNames
      let
        candidates = suggestion : [suggestion <> fromString (show n) | n <- [(1 :: Int)..]]
        (unusedName:_) = filter (not . (`HS.member` usedNames)) candidates
      modify $ \s -> s { builderUsedNames = HS.insert unusedName $ builderUsedNames s }
      return $ Name unusedName

-- | @ir `named` name@ executes the 'IRBuilder' @ir@ using @name@ as the base
-- name whenever a fresh local name is generated. Collisions are avoided by
-- appending numbers (first @"name"@, then @"name1"@, @"name2"@, and so on).
named
  :: MonadIRBuilder m
  => m r
  -> ShortByteString
  -> m r
named ir name = do
  before <- gets builderNameSuggestion
  modify $ \s -> s { builderNameSuggestion = Just name }
  result <- ir
  modify $ \s -> s { builderNameSuggestion = before }
  return result

-- | Emit instruction
emitInstr
  :: MonadIRBuilder m
  => Type -- ^ Return type
  -> Instruction
  -> m Operand
emitInstr retty instr = do
  nm <- fresh
  modifyBlock $ \bb -> bb
    { partialBlockInstrs = partialBlockInstrs bb `snoc` (nm := instr)
    }
  pure (LocalReference retty nm)

-- | Emit terminator
emitTerm
  :: MonadIRBuilder m
  => Terminator
  -> m ()
emitTerm term = modifyBlock $ \bb -> bb
  { partialBlockTerm = Just (Do term)
  }


-------------------------------------------------------------------------------
-- Toplevel
-------------------------------------------------------------------------------

-- | Starts a new block and ends the previous one
block
  :: MonadIRBuilder m
  => m Name
block = do
  nm <- fresh
  bb <- gets builderBlock
  modify $ \s -> s { builderBlock = emptyPartialBlock nm }
  let instrs' = getSnocList $ partialBlockInstrs bb
  case (instrs', partialBlockTerm bb) of
    ([], Nothing) -> return ()
    _ -> do
      let
        newBb = case partialBlockTerm bb of
          Nothing   -> BasicBlock (partialBlockName bb) instrs' (Do (Ret Nothing []))
          Just term -> BasicBlock (partialBlockName bb) instrs' term
      modify $ \s -> s
        { builderBlocks = builderBlocks s `snoc` newBb
        }
  pure nm

-------------------------------------------------------------------------------
-- Instructions
-------------------------------------------------------------------------------

fadd :: MonadIRBuilder m => Operand -> Operand -> m Operand
fadd a b = emitInstr (typeOf a) $ FAdd NoFastMathFlags a b []

fmul :: MonadIRBuilder m => Operand -> Operand -> m Operand
fmul a b = emitInstr (typeOf a) $ FMul NoFastMathFlags a b []

fsub :: MonadIRBuilder m => Operand -> Operand -> m Operand
fsub a b = emitInstr (typeOf a) $ FSub NoFastMathFlags a b []

fdiv :: MonadIRBuilder m => Operand -> Operand -> m Operand
fdiv a b = emitInstr (typeOf a) $ FDiv NoFastMathFlags a b []

frem :: MonadIRBuilder m => Operand -> Operand -> m Operand
frem a b = emitInstr (typeOf a) $ FRem NoFastMathFlags a b []

add :: MonadIRBuilder m => Operand -> Operand -> m Operand
add a b = emitInstr (typeOf a) $ Add False False a b []

mul :: MonadIRBuilder m => Operand -> Operand -> m Operand
mul a b = emitInstr (typeOf a) $ Mul False False a b []

sub :: MonadIRBuilder m => Operand -> Operand -> m Operand
sub a b = emitInstr (typeOf a) $ Sub False False a b []

udiv :: MonadIRBuilder m => Operand -> Operand -> m Operand
udiv a b = emitInstr (typeOf a) $ UDiv True a b []

sdiv :: MonadIRBuilder m => Operand -> Operand -> m Operand
sdiv a b = emitInstr (typeOf a) $ SDiv True a b []

urem :: MonadIRBuilder m => Operand -> Operand -> m Operand
urem a b = emitInstr (typeOf a) $ URem a b []

shl :: MonadIRBuilder m => Operand -> Operand -> m Operand
shl a b = emitInstr (typeOf a) $ Shl False False a b []

lshr :: MonadIRBuilder m => Operand -> Operand -> m Operand
lshr a b = emitInstr (typeOf a) $ LShr True a b []

ashr :: MonadIRBuilder m => Operand -> Operand -> m Operand
ashr a b = emitInstr (typeOf a) $ AShr True a b []

and :: MonadIRBuilder m => Operand -> Operand -> m Operand
and a b = emitInstr (typeOf a) $ And a b []

or :: MonadIRBuilder m => Operand -> Operand -> m Operand
or a b = emitInstr (typeOf a) $ Or a b []

xor :: MonadIRBuilder m => Operand -> Operand -> m Operand
xor a b = emitInstr (typeOf a) $ Xor a b []

alloca :: MonadIRBuilder m => Type -> Maybe Operand -> Word32 -> m Operand
alloca ty count align = emitInstr (ptr ty) $ Alloca ty count align []

load :: MonadIRBuilder m => Operand -> Word32 -> m Operand
load a align = emitInstr (typeOf a) $ Load False a Nothing align []

store :: MonadIRBuilder m => Operand -> Word32 -> Operand -> m Operand
store addr align val = emitInstr (typeOf val) $ Store False addr val Nothing align []

gep :: MonadIRBuilder m => Operand -> [Operand] -> m Operand
gep addr is = emitInstr (gepType (typeOf addr) is) (GetElementPtr False addr is [])

-- TODO: Perhaps use the function from llvm-hs-pretty (https://github.com/llvm-hs/llvm-hs-pretty/blob/master/src/LLVM/Typed.hs)
gepType :: Type -> [Operand] -> Type
gepType ty [] = ptr ty
gepType (PointerType ty _) (_:is) = gepType ty is
gepType (StructureType _ elTys) (ConstantOperand (C.Int 32 val):is) =
  gepType (elTys !! fromIntegral val) is
gepType (StructureType _ _) (i:_) = error $ "gep: Indices into structures should be 32-bit constants. " ++ show i
gepType (VectorType _ elTy) (_:is) = gepType elTy is
gepType (ArrayType _ elTy) (_:is) = gepType elTy is
gepType t (_:_) = error $ "gep: Can't index into a " ++ show t

trunc :: MonadIRBuilder m => Operand -> Type -> m Operand
trunc a to = emitInstr to $ Trunc a to []

zext :: MonadIRBuilder m => Operand -> Type -> m Operand
zext a to = emitInstr to $ ZExt a to []

sext :: MonadIRBuilder m => Operand -> Type -> m Operand
sext a to = emitInstr to $ SExt a to []

fptoui :: MonadIRBuilder m => Operand -> Type -> m Operand
fptoui a to = emitInstr to $ FPToUI a to []

fptosi :: MonadIRBuilder m => Operand -> Type -> m Operand
fptosi a to = emitInstr to $ FPToSI a to []

uitofp :: MonadIRBuilder m => Operand -> Type -> m Operand
uitofp a to = emitInstr to $ UIToFP a to []

sitofp :: MonadIRBuilder m => Operand -> Type -> m Operand
sitofp a to = emitInstr to $ SIToFP a to []

ptrtoint :: MonadIRBuilder m => Operand -> Type -> m Operand
ptrtoint a to = emitInstr to $ PtrToInt a to []

inttoptr :: MonadIRBuilder m => Operand -> Type -> m Operand
inttoptr a to = emitInstr to $ IntToPtr a to []

bitcast :: MonadIRBuilder m => Operand -> Type -> m Operand
bitcast a to = emitInstr to $ BitCast a to []

icmp :: MonadIRBuilder m => IP.IntegerPredicate -> Operand -> Operand -> m Operand
icmp pred a b = emitInstr i1 $ ICmp pred a b []

fcmp :: MonadIRBuilder m => FP.FloatingPointPredicate -> Operand -> Operand -> m Operand
fcmp pred a b = emitInstr i1 $ FCmp pred a b []

-- | Unconditional Branch
br :: MonadIRBuilder m => Name -> m ()
br val = emitTerm (Br val [])

-- | Phi
phi :: MonadIRBuilder m => [(Operand, Name)] -> m Operand
phi [] = emitInstr AST.void $ Phi AST.void [] []
phi incoming@(i:_) = emitInstr ty $ Phi ty incoming []
  where
    ty = typeOf (fst i) -- result type

-- | RetVoid
retVoid :: MonadIRBuilder m => m ()
retVoid = emitTerm (Ret Nothing [])

call :: MonadIRBuilder m => Operand -> [(Operand, [ParameterAttribute])] -> m Operand
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
ret :: MonadIRBuilder m => Operand -> m ()
ret val = emitTerm (Ret (Just val) [])

switch :: MonadIRBuilder m => Operand -> Name -> [(C.Constant, Name)] -> m ()
switch val def dests = emitTerm $ Switch val def dests []

select :: MonadIRBuilder m => Operand -> Operand -> Operand -> m Operand
select cond t f = emitInstr (typeOf t) $ Select cond t f []

condBr :: MonadIRBuilder m => Operand -> Name -> Name -> m ()
condBr cond tdest fdest = emitTerm $ CondBr cond tdest fdest []

-------------------------------------------------------------------------------
-- Module builder
-------------------------------------------------------------------------------

newtype ModuleBuilderT m a = ModuleBuilderT { unModuleBuilderT :: StateT ModuleBuilderState m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadTrans, MonadState ModuleBuilderState)

data ModuleBuilderState = ModuleBuilderState
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

-------------------------------------------------------------------------------
-- Testing
-------------------------------------------------------------------------------

c1 :: Operand
c1 = ConstantOperand $ C.Float (F.Double 10)

c2 :: Operand
c2 = ConstantOperand $ C.Int 32 10

example :: IO ()
example = T.putStrLn $ ppllvm $ mkModule $ runModuleBuilder emptyModuleBuilder $ mdo

  foo <- function "foo" [] double $ \_ -> mdo
    xxx <- fadd c1 c1 `named` "xxx"

    blk1 <- block `named` "blk"; do
      a <- fadd c1 c1
      b <- fadd a a
      c <- add c2 c2
      br blk2

    blk2 <- block `named` "blk"; do
      a <- fadd c1 c1 `named` "c"
      b <- fadd a a
      br blk3

    blk3 <- block `named` "blk"; do
      l <- phi [(c1, blk1), (c1, blk2), (c1, blk3)] `named` "phi"
      a <- fadd c1 c1
      b <- fadd a a
      retVoid

    pure ()


  function "bar" [] double $ \_ -> mdo

    blk3 <- block; do
      a <- fadd c1 c1
      b <- fadd a a
      retVoid

    pure ()

  function "baz" [(double, "arg")] double $ \[arg] -> mdo

    switch c2 blk1 [(C.Int 32 0, blk2), (C.Int 32 1, blk3)]

    blk1 <- block; do
      br blk2

    blk2 <- block; do
      a <- fadd arg c1 `named` "arg"
      b <- fadd a a
      select (cons $ C.Int 1 0) a b
      retVoid

    blk3 <- block; do
      let nul = cons $ C.Null $ ptr $ ptr $ ptr $ IntegerType 32
      addr <- gep nul [cons $ C.Int 32 10, cons $ C.Int 32 20, cons $ C.Int 32 30]
      addr' <- gep addr [cons $ C.Int 32 40]
      retVoid

    pure ()
  where
    mkModule ds = defaultModule { moduleName = "exampleModule", moduleDefinitions = ds }
    cons = ConstantOperand
