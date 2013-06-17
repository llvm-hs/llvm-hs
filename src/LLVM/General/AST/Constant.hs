module LLVM.General.AST.Constant where

import Data.Word (Word32)

import LLVM.General.AST.Type
import LLVM.General.AST.Name
import LLVM.General.AST.FloatingPointPredicate (FloatingPointPredicate)
import LLVM.General.AST.IntegerPredicate (IntegerPredicate)

data Constant
    = Int { constantType :: Type, integerValue :: Integer }
    | Float { constantType :: Type, floatValue :: Double }
    | Null { constantType :: Type }
    | Struct { isPacked :: Bool, memberValues :: [ Constant ] }
    | Array { memberType :: Type, memberValues :: [ Constant ] }
    | Vector { memberValues :: [ Constant ] }
    | Undef { constantType :: Type }
    | BlockAddress { blockAddressFunction :: Name, blockAddressBlock :: Name }
    | GlobalReference Name 
    | Add { 
        operand0 :: Constant,
        operand1 :: Constant
      }
    | FAdd {
        operand0 :: Constant,
        operand1 :: Constant
      }
    | Sub {
        operand0 :: Constant,
        operand1 :: Constant
      }
    | FSub { 
        operand0 :: Constant, 
        operand1 :: Constant
      }
    | Mul { 
        operand0 :: Constant, 
        operand1 :: Constant
      }
    | FMul { 
        operand0 :: Constant, 
        operand1 :: Constant
      }
    | UDiv { 
        operand0 :: Constant, 
        operand1 :: Constant
      }
    | SDiv { 
        operand0 :: Constant, 
        operand1 :: Constant
      }
    | FDiv { 
        operand0 :: Constant, 
        operand1 :: Constant
      }
    | URem { 
        operand0 :: Constant, 
        operand1 :: Constant
      }
    | SRem { 
        operand0 :: Constant, 
        operand1 :: Constant
      }
    | FRem { 
        operand0 :: Constant, 
        operand1 :: Constant
      }
    | Shl { 
        operand0 :: Constant, 
        operand1 :: Constant
      }
    | LShr { 
        operand0 :: Constant, 
        operand1 :: Constant
      }
    | AShr { 
        operand0 :: Constant, 
        operand1 :: Constant
      }
    | And { 
        operand0 :: Constant, 
        operand1 :: Constant
      }
    | Or { 
        operand0 :: Constant, 
        operand1 :: Constant
      }
    | Xor { 
        operand0 :: Constant, 
        operand1 :: Constant
      }
    | GetElementPtr { 
        inBounds :: Bool,
        address :: Constant,
        indices :: [Constant]
      }
    | Trunc { 
        operand0 :: Constant,
        type' :: Type
      }
    | ZExt {
        operand0 :: Constant,
        type' :: Type
      }
    | SExt {
        operand0 :: Constant,
        type' :: Type
      }
    | FPToUI {
        operand0 :: Constant,
        type' :: Type
      }
    | FPToSI {
        operand0 :: Constant,
        type' :: Type
      }
    | UIToFP {
        operand0 :: Constant,
        type' :: Type
      }
    | SIToFP {
        operand0 :: Constant,
        type' :: Type
      }
    | FPTrunc {
        operand0 :: Constant,
        type' :: Type
      }
    | FPExt {
        operand0 :: Constant,
        type' :: Type
      }
    | PtrToInt {
        operand0 :: Constant,
        type' :: Type
      }
    | IntToPtr {
        operand0 :: Constant,
        type' :: Type
      }
    | BitCast {
        operand0 :: Constant,
        type' :: Type
      }
    | ICmp {
        iPredicate :: IntegerPredicate,
        operand0 :: Constant,
        operand1 :: Constant
      }
    | FCmp {
        fpPredicate :: FloatingPointPredicate,
        operand0 :: Constant,
        operand1 :: Constant
      }
    | Select { 
        condition' :: Constant,
        trueValue :: Constant,
        falseValue :: Constant
      }
    | ExtractElement { 
        vector :: Constant,
        index :: Constant
      }
    | InsertElement { 
        vector :: Constant,
        element :: Constant,
        index :: Constant
      }
    | ShuffleVector { 
        operand0 :: Constant,
        operand1 :: Constant,
        mask :: Constant
      }
    | ExtractValue { 
        aggregate :: Constant,
        indices' :: [Word32]
      }
    | InsertValue { 
        aggregate :: Constant,
        element :: Constant,
        indices' :: [Word32]
      }
    deriving (Eq, Ord, Read, Show)
