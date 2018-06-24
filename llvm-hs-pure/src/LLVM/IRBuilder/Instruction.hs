{-# LANGUAGE FlexibleContexts #-}

module LLVM.IRBuilder.Instruction where

import Prelude hiding (and, or, pred)

import Control.Monad.State (gets)
import qualified Data.Map.Lazy as Map
import Data.Word

import LLVM.AST hiding (args, dests)
import LLVM.AST.Type as AST
import LLVM.AST.Typed
import LLVM.AST.ParameterAttribute
import qualified LLVM.AST as AST
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.FloatingPointPredicate as FP

import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Module

fadd :: MonadIRBuilder m => Operand -> Operand -> m Operand
fadd a b = emitInstr (typeOf a) $ FAdd noFastMathFlags a b []

fmul :: MonadIRBuilder m => Operand -> Operand -> m Operand
fmul a b = emitInstr (typeOf a) $ FMul noFastMathFlags a b []

fsub :: MonadIRBuilder m => Operand -> Operand -> m Operand
fsub a b = emitInstr (typeOf a) $ FSub noFastMathFlags a b []

fdiv :: MonadIRBuilder m => Operand -> Operand -> m Operand
fdiv a b = emitInstr (typeOf a) $ FDiv noFastMathFlags a b []

frem :: MonadIRBuilder m => Operand -> Operand -> m Operand
frem a b = emitInstr (typeOf a) $ FRem noFastMathFlags a b []

add :: MonadIRBuilder m => Operand -> Operand -> m Operand
add a b = emitInstr (typeOf a) $ Add False False a b []

mul :: MonadIRBuilder m => Operand -> Operand -> m Operand
mul a b = emitInstr (typeOf a) $ Mul False False a b []

sub :: MonadIRBuilder m => Operand -> Operand -> m Operand
sub a b = emitInstr (typeOf a) $ Sub False False a b []

udiv :: MonadIRBuilder m => Operand -> Operand -> m Operand
udiv a b = emitInstr (typeOf a) $ UDiv False a b []

sdiv :: MonadIRBuilder m => Operand -> Operand -> m Operand
sdiv a b = emitInstr (typeOf a) $ SDiv False a b []

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
load a align = emitInstr retty $ Load False a Nothing align []
  where
    retty = case typeOf a of
      PointerType ty _ -> ty
      _ -> error "Cannot load non-pointer (Malformed AST)."

store :: MonadIRBuilder m => Operand -> Word32 -> Operand -> m ()
store addr align val = emitInstrVoid $ Store False addr val Nothing align []

gep :: (MonadIRBuilder m, MonadModuleBuilder m) => Operand -> [Operand] -> m Operand
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

trunc :: MonadIRBuilder m => Operand -> Type -> m Operand
trunc a to = emitInstr to $ Trunc a to []

fptrunc :: MonadIRBuilder m => Operand -> Type -> m Operand
fptrunc a to = emitInstr to $ FPTrunc a to []

zext :: MonadIRBuilder m => Operand -> Type -> m Operand
zext a to = emitInstr to $ ZExt a to []

sext :: MonadIRBuilder m => Operand -> Type -> m Operand
sext a to = emitInstr to $ SExt a to []

fptoui :: MonadIRBuilder m => Operand -> Type -> m Operand
fptoui a to = emitInstr to $ FPToUI a to []

fptosi :: MonadIRBuilder m => Operand -> Type -> m Operand
fptosi a to = emitInstr to $ FPToSI a to []

fpext :: MonadIRBuilder m => Operand -> Type -> m Operand
fpext a to = emitInstr to $ FPExt a to []

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

extractElement :: MonadIRBuilder m => Operand -> Operand -> m Operand
extractElement v i = emitInstr elemTyp $ ExtractElement v i []
  where elemTyp =
          case typeOf v of
            VectorType _ typ -> typ
            _ -> error "extractElement: Expected a vector type (malformed AST)."

insertElement :: MonadIRBuilder m => Operand -> Operand -> Operand -> m Operand
insertElement v e i = emitInstr (typeOf v) $ InsertElement v e i []

shuffleVector :: MonadIRBuilder m => Operand -> Operand -> C.Constant -> m Operand
shuffleVector a b m = emitInstr retType $ ShuffleVector a b m []
  where retType =
          case (typeOf a, typeOf m) of
            (VectorType _ elemTyp, VectorType maskLength _) -> VectorType maskLength elemTyp
            _ -> error "shuffleVector: Expected two vectors and a vector mask"


extractValue :: MonadIRBuilder m => Operand -> [Word32] -> m Operand
extractValue a i = emitInstr (extractValueType i (typeOf a)) $ ExtractValue a i []

insertValue :: MonadIRBuilder m => Operand -> Operand -> [Word32] -> m Operand
insertValue a e i = emitInstr (typeOf a) $ InsertValue a e i []

icmp :: MonadIRBuilder m => IP.IntegerPredicate -> Operand -> Operand -> m Operand
icmp pred a b = emitInstr i1 $ ICmp pred a b []

fcmp :: MonadIRBuilder m => FP.FloatingPointPredicate -> Operand -> Operand -> m Operand
fcmp pred a b = emitInstr i1 $ FCmp pred a b []

-- | Unconditional branch
br :: MonadIRBuilder m => Name -> m ()
br val = emitTerm (Br val [])

phi :: MonadIRBuilder m => [(Operand, Name)] -> m Operand
phi [] = emitInstr AST.void $ Phi AST.void [] []
phi incoming@(i:_) = emitInstr ty $ Phi ty incoming []
  where
    ty = typeOf (fst i) -- result type

retVoid :: MonadIRBuilder m => m ()
retVoid = emitTerm (Ret Nothing [])

call :: MonadIRBuilder m => Operand -> [(Operand, [ParameterAttribute])] -> m Operand
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

ret :: MonadIRBuilder m => Operand -> m ()
ret val = emitTerm (Ret (Just val) [])

switch :: MonadIRBuilder m => Operand -> Name -> [(C.Constant, Name)] -> m ()
switch val def dests = emitTerm $ Switch val def dests []

select :: MonadIRBuilder m => Operand -> Operand -> Operand -> m Operand
select cond t f = emitInstr (typeOf t) $ Select cond t f []

condBr :: MonadIRBuilder m => Operand -> Name -> Name -> m ()
condBr cond tdest fdest = emitTerm $ CondBr cond tdest fdest []

unreachable :: MonadIRBuilder m => m ()
unreachable = emitTerm $ Unreachable []
