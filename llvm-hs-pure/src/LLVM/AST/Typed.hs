{-# LANGUAGE RecordWildCards #-}

-- | Querying the type of LLVM expressions
module LLVM.AST.Typed (
  Typed(..),
  getElementType,
  getElementPtrType,
  extractValueType,
) where

import LLVM.Prelude

import Control.Monad.State (gets)
import qualified Data.Map.Lazy as Map
import GHC.Stack

import LLVM.AST
import LLVM.AST.Global
import LLVM.AST.Type

import LLVM.IRBuilder.Module

import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F

class Typed a where
    typeOf :: (HasCallStack, MonadModuleBuilder m) => a -> m Type

instance Typed Operand where
  typeOf (LocalReference t _) = return t
  typeOf (ConstantOperand c)  = typeOf c
  typeOf _                    = return MetadataType

instance Typed CallableOperand where
  typeOf (Right op) = typeOf op
  typeOf (Left _) = error "typeOf inline assembler is not defined. (Malformed AST)"

instance Typed C.Constant where
  typeOf (C.Int bits _)  = return $ IntegerType bits
  typeOf (C.Float t) = typeOf t
  typeOf (C.Null t)      = return t
  typeOf (C.AggregateZero t) = return t
  typeOf (C.Struct {..}) = case structName of
                             Nothing -> do
                               mvtys <- mapM typeOf memberValues
                               return $ StructureType isPacked mvtys
                             Just sn -> return $ NamedTypeReference sn
  typeOf (C.Array {..})  = return $ ArrayType (fromIntegral $ length memberValues) memberType
  typeOf (C.Vector {..}) = case memberValues of
                             []    -> error "Vectors of size zero are not allowed. (Malformed AST)"
                             (x:_) -> do
                               t <- typeOf x
                               return $ VectorType (fromIntegral $ length memberValues) t

  typeOf (C.Undef t)     = return t
  typeOf (C.BlockAddress {}) = return $ ptr i8
  typeOf (C.GlobalReference t _) = return t
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
  typeOf (C.GetElementPtr {..}) = do
    aty <- typeOf address
    getElementPtrType aty indices
  typeOf (C.Trunc {..})   = return type'
  typeOf (C.ZExt {..})    = return type'
  typeOf (C.SExt {..})    = return type'
  typeOf (C.FPToUI {..})  = return type'
  typeOf (C.FPToSI {..})  = return type'
  typeOf (C.UIToFP {..})  = return type'
  typeOf (C.SIToFP {..})  = return type'
  typeOf (C.FPTrunc {..}) = return type'
  typeOf (C.FPExt {..})   = return type'
  typeOf (C.PtrToInt {..}) = return type'
  typeOf (C.IntToPtr {..}) = return type'
  typeOf (C.BitCast {..})  = return type'
  typeOf (C.ICmp {..})    = do
    t <- typeOf operand0
    case t of
      (VectorType n _) -> return $ VectorType n i1
      _ -> return i1
  typeOf (C.FCmp {..})    = do
    t <- typeOf operand0
    case t of
      (VectorType n _) -> return $ VectorType n i1
      _ -> return i1
  typeOf (C.Select {..})  = typeOf trueValue
  typeOf (C.ExtractElement {..})  = do
    t <- typeOf vector
    case t of
      (VectorType _ t') -> return t'
      _ -> error "The first operand of an extractelement instruction is a value of vector type. (Malformed AST)"
  typeOf (C.InsertElement {..})   = typeOf vector
  typeOf (C.ShuffleVector {..})   = do
    t0 <- typeOf operand0
    tm <- typeOf mask
    case (t0, tm) of
      (VectorType _ t, VectorType m _) -> return $ VectorType m t
      _ -> error "The first operand of an shufflevector instruction is a value of vector type. (Malformed AST)"
  typeOf (C.ExtractValue {..})    = do
    t <- typeOf aggregate
    extractValueType indices' t
  typeOf (C.InsertValue {..})   = typeOf aggregate
  typeOf (C.TokenNone)          = return TokenType
  typeOf (C.AddrSpaceCast {..}) = return type'

getElementPtrType :: (HasCallStack, MonadModuleBuilder m) => Type -> [C.Constant] -> m Type
getElementPtrType ty [] = return $ ptr ty
getElementPtrType (PointerType ty _) (_:is) = getElementPtrType ty is
getElementPtrType (StructureType _ elTys) (C.Int 32 val:is) =
  getElementPtrType (elTys !! fromIntegral val) is
getElementPtrType (StructureType _ _) (_:_) =
  error "Indices into structures should be 32-bit constants. (Malformed AST)"
getElementPtrType (VectorType _ elTy) (_:is) = getElementPtrType elTy is
getElementPtrType (ArrayType _ elTy) (_:is) = getElementPtrType elTy is
getElementPtrType (NamedTypeReference n) (_:is) = do
  mayTy <- liftModuleState (gets (Map.lookup n . builderTypeDefs))
  case mayTy of
    Nothing -> error $ "Couldn’t resolve typedef for: " ++ show n
    Just ty -> getElementPtrType ty is
getElementPtrType _ _ = error "Expecting aggregate type. (Malformed AST)"

getElementType :: Type -> Type
getElementType (PointerType t _) = t
getElementType _ = error $ "Expecting pointer type. (Malformed AST)"

extractValueType :: (HasCallStack, MonadModuleBuilder m) => [Word32] -> Type -> m Type
extractValueType [] ty = return ty
extractValueType (i : is) (ArrayType numEls elTy)
  | fromIntegral i < numEls = extractValueType is elTy
  | fromIntegral i >= numEls = error "Expecting valid index into array type. (Malformed AST)"
extractValueType (i : is) (StructureType _ elTys)
  | fromIntegral i < length elTys = extractValueType is (elTys !! fromIntegral i)
  | otherwise = error "Expecting valid index into structure type. (Malformed AST)"
extractValueType _ _ = error "Expecting vector type. (Malformed AST)"

instance Typed F.SomeFloat where
  typeOf (F.Half _)          = return $ FloatingPointType HalfFP
  typeOf (F.Single _)        = return $ FloatingPointType FloatFP
  typeOf (F.Double _)        = return $ FloatingPointType DoubleFP
  typeOf (F.Quadruple _ _)   = return $ FloatingPointType FP128FP
  typeOf (F.X86_FP80 _ _)    = return $ FloatingPointType X86_FP80FP
  typeOf (F.PPC_FP128 _ _)   = return $ FloatingPointType PPC_FP128FP

instance Typed Global where
  typeOf (GlobalVariable {..}) = return $ type'
  typeOf (GlobalAlias {..})    = return $ type'
  typeOf (Function {..})       = do
    let (params, isVarArg) = parameters
    ptys <- mapM typeOf params
    return $ FunctionType returnType ptys isVarArg

instance Typed Parameter where
  typeOf (Parameter t _ _) = return t
