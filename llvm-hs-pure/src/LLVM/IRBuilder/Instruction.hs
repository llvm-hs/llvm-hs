{-# LANGUAGE FlexibleContexts #-}

module LLVM.IRBuilder.Instruction where

import Prelude hiding (and, or, pred)

import Control.Monad.State (gets)
import qualified Data.Map.Lazy as Map
import Data.Word
import Data.Char (ord)
import GHC.Stack

import LLVM.AST hiding (args, dests)
import LLVM.AST.Type as AST
import LLVM.AST.Typed
import LLVM.AST.ParameterAttribute
import qualified LLVM.AST as AST
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.FloatingPointPredicate as FP

import LLVM.AST.Global
import LLVM.AST.Linkage

import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Module

-- | See <https://llvm.org/docs/LangRef.html#fadd-instruction reference>.
fadd :: MonadIRBuilder m => Operand -> Operand -> m Operand
fadd a b = emitInstr (typeOf a) $ FAdd noFastMathFlags a b []

-- | See <https://llvm.org/docs/LangRef.html#fmul-instruction reference>.
fmul :: MonadIRBuilder m => Operand -> Operand -> m Operand
fmul a b = emitInstr (typeOf a) $ FMul noFastMathFlags a b []

-- | See <https://llvm.org/docs/LangRef.html#fsub-instruction reference>.
fsub :: MonadIRBuilder m => Operand -> Operand -> m Operand
fsub a b = emitInstr (typeOf a) $ FSub noFastMathFlags a b []

-- | See <https://llvm.org/docs/LangRef.html#fdiv-instruction reference>.
fdiv :: MonadIRBuilder m => Operand -> Operand -> m Operand
fdiv a b = emitInstr (typeOf a) $ FDiv noFastMathFlags a b []

-- | See <https://llvm.org/docs/LangRef.html#frem-instruction reference>.
frem :: MonadIRBuilder m => Operand -> Operand -> m Operand
frem a b = emitInstr (typeOf a) $ FRem noFastMathFlags a b []

-- | See <https://llvm.org/docs/LangRef.html#add-instruction reference>.
add :: MonadIRBuilder m => Operand -> Operand -> m Operand
add a b = emitInstr (typeOf a) $ Add False False a b []

-- | See <https://llvm.org/docs/LangRef.html#mul-instruction reference>.
mul :: MonadIRBuilder m => Operand -> Operand -> m Operand
mul a b = emitInstr (typeOf a) $ Mul False False a b []

-- | See <https://llvm.org/docs/LangRef.html#sub-instruction reference>.
sub :: MonadIRBuilder m => Operand -> Operand -> m Operand
sub a b = emitInstr (typeOf a) $ Sub False False a b []

-- | See <https://llvm.org/docs/LangRef.html#udiv-instruction reference>.
udiv :: MonadIRBuilder m => Operand -> Operand -> m Operand
udiv a b = emitInstr (typeOf a) $ UDiv False a b []

-- | See <https://llvm.org/docs/LangRef.html#sdiv-instruction reference>.
sdiv :: MonadIRBuilder m => Operand -> Operand -> m Operand
sdiv a b = emitInstr (typeOf a) $ SDiv False a b []

-- | See <https://llvm.org/docs/LangRef.html#urem-instruction reference>.
urem :: MonadIRBuilder m => Operand -> Operand -> m Operand
urem a b = emitInstr (typeOf a) $ URem a b []

-- | See <https://llvm.org/docs/LangRef.html#srem-instruction reference>.
srem :: MonadIRBuilder m => Operand -> Operand -> m Operand
srem a b = emitInstr (typeOf a) $ SRem a b []

-- | See <https://llvm.org/docs/LangRef.html#shl-instruction reference>.
shl :: MonadIRBuilder m => Operand -> Operand -> m Operand
shl a b = emitInstr (typeOf a) $ Shl False False a b []

-- | See <https://llvm.org/docs/LangRef.html#lshl-instruction reference>.
lshr :: MonadIRBuilder m => Operand -> Operand -> m Operand
lshr a b = emitInstr (typeOf a) $ LShr True a b []

-- | See <https://llvm.org/docs/LangRef.html#ashl-instruction reference>.
ashr :: MonadIRBuilder m => Operand -> Operand -> m Operand
ashr a b = emitInstr (typeOf a) $ AShr True a b []

-- | See <https://llvm.org/docs/LangRef.html#and-instruction reference>.
and :: MonadIRBuilder m => Operand -> Operand -> m Operand
and a b = emitInstr (typeOf a) $ And a b []

-- | See <https://llvm.org/docs/LangRef.html#or-instruction reference>.
or :: MonadIRBuilder m => Operand -> Operand -> m Operand
or a b = emitInstr (typeOf a) $ Or a b []

-- | See <https://llvm.org/docs/LangRef.html#xor-instruction reference>.
xor :: MonadIRBuilder m => Operand -> Operand -> m Operand
xor a b = emitInstr (typeOf a) $ Xor a b []

-- | See <https://llvm.org/docs/LangRef.html#alloca-instruction reference>.
alloca :: MonadIRBuilder m => Type -> Maybe Operand -> Word32 -> m Operand
alloca ty count align = emitInstr (ptr ty) $ Alloca ty count align []

-- | See <https://llvm.org/docs/LangRef.html#load-instruction reference>.
load :: HasCallStack => MonadIRBuilder m => Operand -> Word32 -> m Operand
load a align = emitInstr retty $ Load False a Nothing align []
  where
    retty = case typeOf a of
      PointerType ty _ -> ty
      _ -> error "Cannot load non-pointer (Malformed AST)."

-- | See <https://llvm.org/docs/LangRef.html#store-instruction reference>.
store :: MonadIRBuilder m => Operand -> Word32 -> Operand -> m ()
store addr align val = emitInstrVoid $ Store False addr val Nothing align []

-- | Emit the @getelementptr@ instruction.
-- See <https://llvm.org/docs/LangRef.html#getelementptr-instruction reference>.
gep :: (MonadIRBuilder m, MonadModuleBuilder m, HasCallStack) => Operand -> [Operand] -> m Operand
gep addr is = do
  ty <- gepType (typeOf addr) is
  emitInstr ty (GetElementPtr False addr is [])
  where
    -- TODO: Perhaps use the function from llvm-hs-pretty (https://github.com/llvm-hs/llvm-hs-pretty/blob/master/src/LLVM/Typed.hs)
    gepType :: MonadModuleBuilder m => Type -> [Operand] -> m Type
    gepType ty [] = pure (ptr ty)
    gepType (PointerType ty _) (_:is') = gepType ty is'
    gepType (StructureType _ elTys) (ConstantOperand (C.Int 32 val):is') =
      gepType (elTys !! fromIntegral val) is'
    gepType (StructureType _ _) (i:_) = error $ "gep: Indices into structures should be 32-bit constants. " ++ show i
    gepType (VectorType _ elTy) (_:is') = gepType elTy is'
    gepType (ArrayType _ elTy) (_:is') = gepType elTy is'
    gepType (NamedTypeReference nm) is' = do
      mayTy <- liftModuleState (gets (Map.lookup nm . builderTypeDefs))
      case mayTy of
        Nothing -> error $ "gep: Couldn’t resolve typedef for: " ++ show nm
        Just ty -> gepType ty is'
    gepType t (_:_) = error $ "gep: Can't index into a " ++ show t

-- | Emit the @trunc ... to@ instruction.
-- See <https://llvm.org/docs/LangRef.html#trunc-to-instruction reference>.
trunc :: MonadIRBuilder m => Operand -> Type -> m Operand
trunc a to = emitInstr to $ Trunc a to []

-- | Emit the @fptrunc ... to@ instruction.
-- See <https://llvm.org/docs/LangRef.html#fptrunc-to-instruction reference>.
fptrunc :: MonadIRBuilder m => Operand -> Type -> m Operand
fptrunc a to = emitInstr to $ FPTrunc a to []

-- | Emit the @zext ... to@ instruction.
-- See <https://llvm.org/docs/LangRef.html#zext-to-instruction reference>.
zext :: MonadIRBuilder m => Operand -> Type -> m Operand
zext a to = emitInstr to $ ZExt a to []

-- | Emit the @sext ... to@ instruction.
-- See <https://llvm.org/docs/LangRef.html#sext-to-instruction reference>.
sext :: MonadIRBuilder m => Operand -> Type -> m Operand
sext a to = emitInstr to $ SExt a to []

-- | Emit the @fptoui ... to@ instruction.
-- See <https://llvm.org/docs/LangRef.html#fptoui-to-instruction reference>.
fptoui :: MonadIRBuilder m => Operand -> Type -> m Operand
fptoui a to = emitInstr to $ FPToUI a to []

-- | Emit the @fptosi ... to@ instruction.
-- See <https://llvm.org/docs/LangRef.html#fptosi-to-instruction reference>.
fptosi :: MonadIRBuilder m => Operand -> Type -> m Operand
fptosi a to = emitInstr to $ FPToSI a to []

-- | Emit the @fpext ... to@ instruction.
-- See <https://llvm.org/docs/LangRef.html#fpext-to-instruction reference>.
fpext :: MonadIRBuilder m => Operand -> Type -> m Operand
fpext a to = emitInstr to $ FPExt a to []

-- | Emit the @uitofp ... to@ instruction.
-- See <https://llvm.org/docs/LangRef.html#uitofp-to-instruction reference>.
uitofp :: MonadIRBuilder m => Operand -> Type -> m Operand
uitofp a to = emitInstr to $ UIToFP a to []

-- | Emit the @sitofp ... to@ instruction.
-- See <https://llvm.org/docs/LangRef.html#sitofp-to-instruction reference>.
sitofp :: MonadIRBuilder m => Operand -> Type -> m Operand
sitofp a to = emitInstr to $ SIToFP a to []

-- | Emit the @ptrtoint ... to@ instruction.
-- See <https://llvm.org/docs/LangRef.html#ptrtoint-to-instruction reference>.
ptrtoint :: MonadIRBuilder m => Operand -> Type -> m Operand
ptrtoint a to = emitInstr to $ PtrToInt a to []

-- | Emit the @inttoptr ... to@ instruction.
-- See <https://llvm.org/docs/LangRef.html#inttoptr-to-instruction reference>.
inttoptr :: MonadIRBuilder m => Operand -> Type -> m Operand
inttoptr a to = emitInstr to $ IntToPtr a to []

-- | Emit the @bitcast ... to@ instruction.
-- See <https://llvm.org/docs/LangRef.html#bitcast-to-instruction reference>.
bitcast :: MonadIRBuilder m => Operand -> Type -> m Operand
bitcast a to = emitInstr to $ BitCast a to []

-- | See <https://llvm.org/docs/LangRef.html#extractelement-instruction reference>.
extractElement :: (MonadIRBuilder m, HasCallStack) => Operand -> Operand -> m Operand
extractElement v i = emitInstr elemTyp $ ExtractElement v i []
  where elemTyp =
          case typeOf v of
            VectorType _ typ -> typ
            _ -> error "extractElement: Expected a vector type (malformed AST)."

-- | See <https://llvm.org/docs/LangRef.html#insertelement-instruction reference>.
insertElement :: MonadIRBuilder m => Operand -> Operand -> Operand -> m Operand
insertElement v e i = emitInstr (typeOf v) $ InsertElement v e i []

-- | See <https://llvm.org/docs/LangRef.html#shufflevector-instruction reference>.
shuffleVector :: (MonadIRBuilder m, HasCallStack) => Operand -> Operand -> C.Constant -> m Operand
shuffleVector a b m = emitInstr retType $ ShuffleVector a b m []
  where retType =
          case (typeOf a, typeOf m) of
            (VectorType _ elemTyp, VectorType maskLength _) -> VectorType maskLength elemTyp
            _ -> error "shuffleVector: Expected two vectors and a vector mask"


-- | See <https://llvm.org/docs/LangRef.html#extractvalue-instruction reference>.
extractValue :: MonadIRBuilder m => Operand -> [Word32] -> m Operand
extractValue a i = emitInstr (extractValueType i (typeOf a)) $ ExtractValue a i []

-- | See <https://llvm.org/docs/LangRef.html#insertvalue-instruction reference>.
insertValue :: MonadIRBuilder m => Operand -> Operand -> [Word32] -> m Operand
insertValue a e i = emitInstr (typeOf a) $ InsertValue a e i []

-- | See <https://llvm.org/docs/LangRef.html#icmp-instruction reference>.
icmp :: MonadIRBuilder m => IP.IntegerPredicate -> Operand -> Operand -> m Operand
icmp pred a b = emitInstr i1 $ ICmp pred a b []

-- | See <https://llvm.org/docs/LangRef.html#fcmp-instruction reference>.
fcmp :: MonadIRBuilder m => FP.FloatingPointPredicate -> Operand -> Operand -> m Operand
fcmp pred a b = emitInstr i1 $ FCmp pred a b []

-- | Unconditional branch.
-- Emit a @br label <dest>@ instruction
-- See <https://llvm.org/docs/LangRef.html#br-instruction reference>.
br :: MonadIRBuilder m => Name -> m ()
br val = emitTerm (Br val [])

-- | See <https://llvm.org/docs/LangRef.html#phi-instruction reference>.
phi :: MonadIRBuilder m => [(Operand, Name)] -> m Operand
phi [] = emitInstr AST.void $ Phi AST.void [] []
phi incoming@(i:_) = emitInstr ty $ Phi ty incoming []
  where
    ty = typeOf (fst i) -- result type

-- | Emit a @ret void@ instruction.
-- See <https://llvm.org/docs/LangRef.html#ret-instruction reference>.
retVoid :: MonadIRBuilder m => m ()
retVoid = emitTerm (Ret Nothing [])

-- | See <https://llvm.org/docs/LangRef.html#call-instruction reference>.
call :: (MonadIRBuilder m, HasCallStack) => Operand -> [(Operand, [ParameterAttribute])] -> m Operand
call fun args = do
  let instr = Call {
    AST.tailCallKind = Nothing
  , AST.callingConvention = CC.C
  , AST.returnAttributes = []
  , AST.function = Right fun
  , AST.arguments = args
  , AST.functionAttributes = []
  , AST.metadata = []
  }
  case typeOf fun of
      FunctionType r _ _ -> case r of
        VoidType -> emitInstrVoid instr >> (pure (ConstantOperand (C.Undef void)))
        _        -> emitInstr r instr
      PointerType (FunctionType r _ _) _ -> case r of
        VoidType -> emitInstrVoid instr >> (pure (ConstantOperand (C.Undef void)))
        _        -> emitInstr r instr
      _ -> error "Cannot call non-function (Malformed AST)."

-- | See <https://llvm.org/docs/LangRef.html#ret-instruction reference>.
ret :: MonadIRBuilder m => Operand -> m ()
ret val = emitTerm (Ret (Just val) [])

-- | See <https://llvm.org/docs/LangRef.html#switch-instruction reference>.
switch :: MonadIRBuilder m => Operand -> Name -> [(C.Constant, Name)] -> m ()
switch val def dests = emitTerm $ Switch val def dests []

-- | See <https://llvm.org/docs/LangRef.html#select-instruction reference>.
select :: MonadIRBuilder m => Operand -> Operand -> Operand -> m Operand
select cond t f = emitInstr (typeOf t) $ Select cond t f []

-- | Conditional branch (see 'br' for unconditional instructions).
-- See <https://llvm.org/docs/LangRef.html#br-instruction reference>.
condBr :: MonadIRBuilder m => Operand -> Name -> Name -> m ()
condBr cond tdest fdest = emitTerm $ CondBr cond tdest fdest []

-- | See <https://llvm.org/docs/LangRef.html#unreachable-instruction reference>.
unreachable :: MonadIRBuilder m => m ()
unreachable = emitTerm $ Unreachable []

-- | Creates a series of instructions to generate a pointer to a string
-- constant. Useful for making format strings to pass to @printf@, for example
globalStringPtr
  :: (MonadModuleBuilder m)
  => String       -- ^ The string to generate
  -> Name         -- ^ Variable name of the pointer
  -> m C.Constant
globalStringPtr str nm = do
  let asciiVals = map (fromIntegral . ord) str
      llvmVals  = map (C.Int 8) (asciiVals ++ [0]) -- append null terminator
      char      = IntegerType 8
      charArray = C.Array char llvmVals
      ty        = LLVM.AST.Typed.typeOf charArray
  emitDefn $ GlobalDefinition globalVariableDefaults
    { name                  = nm
    , LLVM.AST.Global.type' = ty
    , linkage               = External
    , isConstant            = True
    , initializer           = Just charArray
    , unnamedAddr           = Just GlobalAddr
    }
  return $ C.GetElementPtr True
                           (C.GlobalReference (ptr ty) nm)
                           [(C.Int 32 0), (C.Int 32 0)]
