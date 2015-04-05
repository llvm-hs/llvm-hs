-- | A representation of LLVM constants
module LLVM.General.AST.Constant where

import LLVM.General.Prelude

import Data.Bits ((.|.), (.&.), complement, testBit, shiftL)

import LLVM.General.AST.Type
import LLVM.General.AST.Name
import LLVM.General.AST.FloatingPointPredicate (FloatingPointPredicate)
import LLVM.General.AST.IntegerPredicate (IntegerPredicate)
import qualified LLVM.General.AST.Float as F

{- |
<http://llvm.org/docs/LangRef.html#constants>

N.B. - <http://llvm.org/docs/LangRef.html#constant-expressions>

Although constant expressions and instructions have many similarites, there are important
differences - so they're represented using different types in this AST. At the cost of making it
harder to move an code back and forth between being constant and not, this approach embeds more of
the rules of what IR is legal into the Haskell types.
-} 
data Constant
    = Int { integerBits :: Word32, integerValue :: Integer }
    | Float { floatValue :: F.SomeFloat }
    | Null { constantType :: Type }
    | Struct { structName :: Maybe Name, isPacked :: Bool, memberValues :: [ Constant ] }
    | Array { memberType :: Type, memberValues :: [ Constant ] }
    | Vector { memberValues :: [ Constant ] }
    | Undef { constantType :: Type }
    | BlockAddress { blockAddressFunction :: Name, blockAddressBlock :: Name }
    | GlobalReference Type Name
    | Add { 
        nsw :: Bool,
        nuw :: Bool,
        operand0 :: Constant,
        operand1 :: Constant
      }
    | FAdd {
        operand0 :: Constant,
        operand1 :: Constant
      }
    | Sub {
        nsw :: Bool,
        nuw :: Bool,
        operand0 :: Constant,
        operand1 :: Constant
      }
    | FSub { 
        operand0 :: Constant, 
        operand1 :: Constant
      }
    | Mul { 
        nsw :: Bool,
        nuw :: Bool,
        operand0 :: Constant, 
        operand1 :: Constant
      }
    | FMul { 
        operand0 :: Constant, 
        operand1 :: Constant
      }
    | UDiv { 
        exact :: Bool,
        operand0 :: Constant, 
        operand1 :: Constant
      }
    | SDiv { 
        exact :: Bool,
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
        nsw :: Bool,
        nuw :: Bool,
        operand0 :: Constant, 
        operand1 :: Constant
      }
    | LShr { 
        exact :: Bool,
        operand0 :: Constant, 
        operand1 :: Constant
      }
    | AShr { 
        exact :: Bool,
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
    deriving (Eq, Ord, Read, Show, Typeable, Data)


-- | Since LLVM types don't include signedness, there's ambiguity in interpreting
-- an constant as an Integer. The LLVM assembly printer prints integers as signed, but
-- cheats for 1-bit integers and prints them as 'true' or 'false'. That way it circuments the
-- otherwise awkward fact that a twos complement 1-bit number only has the values -1 and 0.
signedIntegerValue :: Constant -> Integer
signedIntegerValue (Int nBits' bits) =
  let nBits = fromIntegral nBits'
  in
    if bits `testBit` (nBits - 1) then bits .|. (-1 `shiftL` nBits) else bits

-- | This library's conversion from LLVM C++ objects will always produce integer constants
-- as unsigned, so this function in many cases is not necessary. However, nothing's to keep
-- stop direct construction of an 'Int' with a negative 'integerValue'. There's nothing in principle
-- wrong with such a value - it has perfectly good low order bits like any integer, and will be used
-- as such, likely producing the intended result if lowered to C++. If, however one wishes to interpret
-- an 'Int' of unknown provenance as unsigned, then this function will serve.
unsignedIntegerValue :: Constant -> Integer
unsignedIntegerValue (Int nBits bits) =
  bits .&. (complement (-1 `shiftL` (fromIntegral nBits)))

