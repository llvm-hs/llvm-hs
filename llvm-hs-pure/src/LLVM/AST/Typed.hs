{-# LANGUAGE RecordWildCards #-}

-- | Querying the type of LLVM expressions
module LLVM.AST.Typed (
  Typed(..),
  getElementType,
  indexTypeByConstants,
  indexTypeByOperands,
  extractValueType,
) where

import LLVM.Prelude

import Control.Monad.State (gets)
import qualified Data.Map.Lazy as Map
import qualified Data.Either as Either
import GHC.Stack

import LLVM.AST
import LLVM.AST.Global
import LLVM.AST.Type

import LLVM.IRBuilder.Module

import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F

class Typed a where
  typeOf :: (HasCallStack, MonadModuleBuilder m) => a -> m (Either String Type)

instance Typed Operand where
  typeOf (LocalReference t _) = return $ Right t
  typeOf (ConstantOperand c)  = typeOf c
  typeOf _                    = return $ Right MetadataType

instance Typed CallableOperand where
  typeOf (Right op) = typeOf op
  typeOf (Left _) = return $ Left "typeOf inline assembler is not defined. (Malformed AST)"

instance Typed C.Constant where
  typeOf (C.Int bits _)       = return $ Right $ IntegerType bits
  typeOf (C.Float t)          = typeOf t
  typeOf (C.Null t)           = return $ Right t
  typeOf (C.AggregateZero t)  = return $ Right t
  typeOf (C.Struct {..}) = case structName of
                             Nothing -> do
                               mvtys <- mapM typeOf memberValues
                               case (all Either.isRight mvtys) of
                                 True -> return $ Right $ StructureType isPacked $ Either.rights mvtys
                                 False -> do
                                   let (Left s) = head $ filter Either.isLeft mvtys
                                   return $ Left $ "Could not deduce type for struct field: " ++ s
                             Just sn -> return $ Right $ NamedTypeReference sn
  typeOf (C.Array {..})  = return $ Right $ ArrayType (fromIntegral $ length memberValues) memberType
  typeOf (C.Vector {..}) = case memberValues of
                             []    -> return $ Left "Vectors of size zero are not allowed. (Malformed AST)"
                             (x:_) -> do
                               t <- typeOf x
                               case t of
                                 (Left _) -> return t
                                 (Right t') -> return $ Right $ VectorType (fromIntegral $ length memberValues) t'

  typeOf (C.Undef t)     = return $ Right t
  typeOf (C.BlockAddress {}) = return $ Right $ ptr i8
  typeOf (C.GlobalReference t _) = return $ Right t
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
    case aty of
      (Left _) -> return aty
      (Right aty') -> indexTypeByConstants aty' indices
  typeOf (C.Trunc {..})    = return $ Right type'
  typeOf (C.ZExt {..})     = return $ Right type'
  typeOf (C.SExt {..})     = return $ Right type'
  typeOf (C.FPToUI {..})   = return $ Right type'
  typeOf (C.FPToSI {..})   = return $ Right type'
  typeOf (C.UIToFP {..})   = return $ Right type'
  typeOf (C.SIToFP {..})   = return $ Right type'
  typeOf (C.FPTrunc {..})  = return $ Right type'
  typeOf (C.FPExt {..})    = return $ Right type'
  typeOf (C.PtrToInt {..}) = return $ Right type'
  typeOf (C.IntToPtr {..}) = return $ Right type'
  typeOf (C.BitCast {..})  = return $ Right type'
  typeOf (C.ICmp {..})    = do
    t <- typeOf operand0
    case t of
      (Left _) -> return t
      (Right (VectorType n _)) -> return $ Right $ VectorType n i1
      (Right _) -> return $ Right i1
  typeOf (C.FCmp {..})    = do
    t <- typeOf operand0
    case t of
      (Left _) -> return t
      (Right (VectorType n _)) -> return $ Right $ VectorType n i1
      (Right _) -> return $ Right i1
  typeOf (C.Select {..})  = typeOf trueValue
  typeOf (C.ExtractElement {..})  = do
    t <- typeOf vector
    case t of
      (Left _) -> return t
      (Right (VectorType _ t')) -> return $ Right t'
      (Right _) -> return $ Left "The first operand of an extractelement instruction is a value of vector type. (Malformed AST)"
  typeOf (C.InsertElement {..})   = typeOf vector
  typeOf (C.ShuffleVector {..})   = do
    t0 <- typeOf operand0
    tm <- typeOf mask
    case (t0, tm) of
      (Right (VectorType _ t), Right (VectorType m _)) -> return $ Right $ VectorType m t
      _ -> return $ Left "The first operand of an shufflevector instruction is a value of vector type. (Malformed AST)"
  typeOf (C.ExtractValue {..})    = do
    t <- typeOf aggregate
    case t of
      (Left _) -> return t
      (Right t') -> extractValueType indices' t'
  typeOf (C.InsertValue {..})   = typeOf aggregate
  typeOf (C.TokenNone)          = return $ Right TokenType
  typeOf (C.AddrSpaceCast {..}) = return $ Right type'

-- | Index into a type using a list of 'Constant' values. Returns a pointer type whose referent is the indexed type, or an error message if indexing was not possible.
indexTypeByConstants :: (HasCallStack, MonadModuleBuilder m) => Type -> [C.Constant] -> m (Either String Type)
indexTypeByConstants ty [] = return $ Right $ ptr ty
indexTypeByConstants (PointerType ty _) (_:is) = indexTypeByConstants ty is
indexTypeByConstants (StructureType _ elTys) (C.Int 32 val:is) =
  indexTypeByConstants (elTys !! fromIntegral val) is
indexTypeByConstants (StructureType _ _) (i:_) =
  return $ Left $ "Indices into structures should be 32-bit integer constants. (Malformed AST): " ++ show i
indexTypeByConstants (VectorType _ elTy) (_:is) = indexTypeByConstants elTy is
indexTypeByConstants (ArrayType _ elTy) (_:is) = indexTypeByConstants elTy is
indexTypeByConstants (NamedTypeReference n) is = do
  mayTy <- liftModuleState (gets (Map.lookup n . builderTypeDefs))
  case mayTy of
    Nothing -> return $ Left $ "Couldn’t resolve typedef for: " ++ show n
    Just ty -> indexTypeByConstants ty is
indexTypeByConstants ty _ = return $ Left $ "Expecting aggregate type. (Malformed AST): " ++ show ty

-- | Index into a type using a list of 'Operand' values. Returns a pointer type whose referent is the indexed type, or an error message if indexing was not possible.
indexTypeByOperands :: (HasCallStack, MonadModuleBuilder m) => Type -> [Operand] -> m (Either String Type)
indexTypeByOperands ty [] = return $ Right $ ptr ty
indexTypeByOperands (PointerType ty _) (_:is) = indexTypeByOperands ty is
indexTypeByOperands (StructureType _ elTys) (ConstantOperand (C.Int 32 val):is) =
  indexTypeByOperands (elTys !! fromIntegral val) is
indexTypeByOperands (StructureType _ _) (i:_) =
  return $ Left $ "Indices into structures should be 32-bit integer constants. (Malformed AST): " ++ show i
indexTypeByOperands (VectorType _ elTy) (_:is) = indexTypeByOperands elTy is
indexTypeByOperands (ArrayType _ elTy) (_:is) = indexTypeByOperands elTy is
indexTypeByOperands (NamedTypeReference n) is = do
  mayTy <- liftModuleState (gets (Map.lookup n . builderTypeDefs))
  case mayTy of
    Nothing -> return $ Left $ "Couldn’t resolve typedef for: " ++ show n
    Just ty -> indexTypeByOperands ty is
indexTypeByOperands ty _ = return $ Left $ "Expecting aggregate type. (Malformed AST): " ++ show ty

getElementType :: Type -> Either String Type
getElementType (PointerType t _) = Right t
getElementType t = Left $ "Expecting pointer type. (Malformed AST): " ++ show t

extractValueType :: (HasCallStack, MonadModuleBuilder m) => [Word32] -> Type -> m (Either String Type)
extractValueType [] ty = return $ Right ty
extractValueType (i : is) (ArrayType numEls elTy)
  | fromIntegral i < numEls = extractValueType is elTy
  | fromIntegral i >= numEls = return $ Left $ "Expecting valid index into array type. (Malformed AST): " ++ show i
extractValueType (i : is) (StructureType _ elTys)
  | fromIntegral i < length elTys = extractValueType is (elTys !! fromIntegral i)
  | otherwise = return $ Left $ "Expecting valid index into structure type. (Malformed AST): " ++ show i
extractValueType _ ty = return $ Left $ "Expecting vector type. (Malformed AST): " ++ show ty

instance Typed F.SomeFloat where
  typeOf (F.Half _)          = return $ Right $ FloatingPointType HalfFP
  typeOf (F.Single _)        = return $ Right $ FloatingPointType FloatFP
  typeOf (F.Double _)        = return $ Right $ FloatingPointType DoubleFP
  typeOf (F.Quadruple _ _)   = return $ Right $ FloatingPointType FP128FP
  typeOf (F.X86_FP80 _ _)    = return $ Right $ FloatingPointType X86_FP80FP
  typeOf (F.PPC_FP128 _ _)   = return $ Right $ FloatingPointType PPC_FP128FP

instance Typed Global where
  typeOf (GlobalVariable {..}) = return $ Right $ type'
  typeOf (GlobalAlias {..})    = return $ Right $ type'
  typeOf (Function {..})       = do
    let (params, isVarArg) = parameters
    ptys <- mapM typeOf params
    case (all Either.isRight ptys) of
      True -> return $ Right $ FunctionType returnType (Either.rights ptys) isVarArg
      False -> do
        let (Left s) = head $ filter Either.isLeft ptys
        return $ Left $ "Could not deduce type for function parameter: " ++ s

instance Typed Parameter where
  typeOf (Parameter t _ _) = return $ Right t
