{-# LANGUAGE RecordWildCards #-}

-- | Querying the type of LLVM expressions
module LLVM.AST.Typed (
  Typed(..),
  getElementType,
  getElementPtrType,
  extractValueType,
) where

import LLVM.Prelude

import GHC.Stack

import LLVM.AST
import LLVM.AST.Global
import LLVM.AST.Type

import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F

class Typed a where
    typeOf :: HasCallStack => a -> Type

instance Typed Operand where
  typeOf (LocalReference t _) = t
  typeOf (ConstantOperand c)  = typeOf c
  typeOf _                    = MetadataType

instance Typed CallableOperand where
  typeOf (Right op) = typeOf op
  typeOf (Left _) = error "typeOf inline assembler is not defined. (Malformed AST)"

instance Typed C.Constant where
  typeOf (C.Int bits _)  = IntegerType bits
  typeOf (C.Float t) = typeOf t
  typeOf (C.Null t)      = t
  typeOf (C.AggregateZero t) = t
  typeOf (C.Struct {..}) = StructureType isPacked (map typeOf memberValues)
  typeOf (C.Array {..})  = ArrayType (fromIntegral $ length memberValues) memberType
  typeOf (C.Vector {..}) = VectorType (fromIntegral $ length memberValues) $
                              case memberValues of
                                  []    -> error "Vectors of size zero are not allowed. (Malformed AST)"
                                  (x:_) -> typeOf x
  typeOf (C.Undef t)     = t
  typeOf (C.BlockAddress {..})   = ptr i8
  typeOf (C.GlobalReference t _) = t
  typeOf (C.Add {..})     = typeOf operand0
  typeOf (C.FAdd {..})    = typeOf operand0
  typeOf (C.FDiv {..})    = typeOf operand0
  typeOf (C.FRem {..})    = typeOf operand0
  typeOf (C.Sub {..})     = typeOf operand0
  typeOf (C.FSub {..})    = typeOf operand0
  typeOf (C.Mul {..})     = typeOf operand0
  typeOf (C.FMul {..})    = typeOf operand0
  typeOf (C.UDiv {..})    = typeOf operand0
  typeOf (C.SDiv {..})    = typeOf operand0
  typeOf (C.URem {..})    = typeOf operand0
  typeOf (C.SRem {..})    = typeOf operand0
  typeOf (C.Shl {..})     = typeOf operand0
  typeOf (C.LShr {..})    = typeOf operand0
  typeOf (C.AShr {..})    = typeOf operand0
  typeOf (C.And {..})     = typeOf operand0
  typeOf (C.Or  {..})     = typeOf operand0
  typeOf (C.Xor {..})     = typeOf operand0
  typeOf (C.GetElementPtr {..}) = getElementPtrType (typeOf address) indices
  typeOf (C.Trunc {..})   = type'
  typeOf (C.ZExt {..})    = type'
  typeOf (C.SExt {..})    = type'
  typeOf (C.FPToUI {..})  = type'
  typeOf (C.FPToSI {..})  = type'
  typeOf (C.UIToFP {..})  = type'
  typeOf (C.SIToFP {..})  = type'
  typeOf (C.FPTrunc {..}) = type'
  typeOf (C.FPExt {..})   = type'
  typeOf (C.PtrToInt {..}) = type'
  typeOf (C.IntToPtr {..}) = type'
  typeOf (C.BitCast {..})  = type'
  typeOf (C.ICmp {..})    = case (typeOf operand0) of
                              (VectorType n _) -> VectorType n i1
                              _ -> i1
  typeOf (C.FCmp {..})    = case (typeOf operand0) of
                              (VectorType n _) -> VectorType n i1
                              _ -> i1
  typeOf (C.Select {..})  = typeOf trueValue
  typeOf (C.ExtractElement {..})  = case typeOf vector of
                                      (VectorType _ t) -> t
                                      _ -> error "The first operand of an extractelement instruction is a value of vector type. (Malformed AST)"
  typeOf (C.InsertElement {..})   = typeOf vector
  typeOf (C.ShuffleVector {..})   = case (typeOf operand0, typeOf mask) of
                                      (VectorType _ t, VectorType m _) -> VectorType m t
                                      _ -> error "The first operand of an shufflevector instruction is a value of vector type. (Malformed AST)"
  typeOf (C.ExtractValue {..})    = extractValueType indices' (typeOf aggregate)
  typeOf (C.InsertValue {..})     = typeOf aggregate
  typeOf (C.TokenNone)          = TokenType
  typeOf (C.AddrSpaceCast {..}) = type'

getElementPtrType :: Type -> [C.Constant] -> Type
getElementPtrType ty [] = ptr ty
getElementPtrType (PointerType ty _) (_:is) = getElementPtrType ty is
getElementPtrType (StructureType _ elTys) (C.Int 32 val:is) =
  getElementPtrType (elTys !! fromIntegral val) is
getElementPtrType (VectorType _ elTy) (_:is) = getElementPtrType elTy is
getElementPtrType (ArrayType _ elTy) (_:is) = getElementPtrType elTy is
getElementPtrType _ _ = error "Expecting aggregate type. (Malformed AST)"

getElementType :: Type -> Type
getElementType (PointerType t _) = t
getElementType _ = error $ "Expecting pointer type. (Malformed AST)"

extractValueType :: [Word32] -> Type -> Type
extractValueType [] ty = ty
extractValueType (i : is) (ArrayType numEls elTy)
  | fromIntegral i < numEls = extractValueType is elTy
  | fromIntegral i >= numEls = error "Expecting valid index into array type. (Malformed AST)"
extractValueType (i : is) (StructureType _ elTys)
  | fromIntegral i < length elTys = extractValueType is (elTys !! fromIntegral i)
  | otherwise = error "Expecting valid index into structure type. (Malformed AST)"
extractValueType _ _ = error "Expecting vector type. (Malformed AST)"

instance Typed F.SomeFloat where
  typeOf (F.Half _)          = FloatingPointType HalfFP
  typeOf (F.Single _)        = FloatingPointType FloatFP
  typeOf (F.Double _)        = FloatingPointType DoubleFP
  typeOf (F.Quadruple _ _)   = FloatingPointType FP128FP
  typeOf (F.X86_FP80 _ _)    = FloatingPointType X86_FP80FP
  typeOf (F.PPC_FP128 _ _)   = FloatingPointType PPC_FP128FP

instance Typed Global where
  typeOf (GlobalVariable {..}) = type'
  typeOf (GlobalAlias {..})    = type'
  typeOf (Function {..})       = let (params, isVarArg) = parameters
                                   in FunctionType returnType (map typeOf params) isVarArg
instance Typed Parameter where
  typeOf (Parameter t _ _) = t
