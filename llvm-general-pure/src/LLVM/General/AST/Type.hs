-- | A representation of an LLVM type
module LLVM.General.AST.Type where

import Data.Data
import Data.Word (Word32, Word64)

import LLVM.General.AST.AddrSpace
import LLVM.General.AST.Name

-- | LLVM supports some special formats floating point format. This type is to distinguish those format.
-- I believe it's treated as a format for "a" float, as opposed to a vector of two floats, because
-- its intended usage is to represent a single number with a combined significand.
data FloatingPointFormat
  = IEEE
  | DoubleExtended
  | PairOfFloats
  deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | <http://llvm.org/docs/LangRef.html#type-system>
data Type
  -- | <http://llvm.org/docs/LangRef.html#void-type>
  = VoidType
  -- | <http://llvm.org/docs/LangRef.html#integer-type>
  | IntegerType { typeBits :: Word32 }
  -- | <http://llvm.org/docs/LangRef.html#pointer-type>
  | PointerType { pointerReferent :: Type, pointerAddrSpace :: AddrSpace }
  -- | <http://llvm.org/docs/LangRef.html#floating-point-types>
  | FloatingPointType { typeBits :: Word32, floatingPointFormat :: FloatingPointFormat }
  -- | <http://llvm.org/docs/LangRef.html#function-type>
  | FunctionType { resultType :: Type, argumentTypes :: [Type], isVarArg :: Bool }
  -- | <http://llvm.org/docs/LangRef.html#vector-type>
  | VectorType { nVectorElements :: Word32, elementType :: Type }
  -- | <http://llvm.org/docs/LangRef.html#structure-type>
  | StructureType { isPacked :: Bool, elementTypes :: [Type] }
  -- | <http://llvm.org/docs/LangRef.html#array-type>
  | ArrayType { nArrayElements :: Word64, elementType :: Type }
  -- | <http://llvm.org/docs/LangRef.html#opaque-structure-types>
  | NamedTypeReference Name
  -- | <http://llvm.org/docs/LangRef.html#metadata-type>
  | MetadataType -- only to be used as a parameter type for a few intrinsics
  deriving (Eq, Ord, Read, Show, Typeable, Data)

void = VoidType -- ^ An abbreviation for VoidType
i1 = IntegerType 1 -- ^ An abbreviation for IntegerType 1
i8 = IntegerType 8 -- ^ An abbreviation for IntegerType 8
i16 = IntegerType 16 -- ^ An abbreviation for IntegerType 16
i32 = IntegerType 32 -- ^ An abbreviation for IntegerType 32
i64 = IntegerType 64 -- ^ An abbreviation for IntegerType 64
i128 = IntegerType 128 -- ^ An abbreviation for IntegerType 128
ptr t = PointerType t (AddrSpace 0) -- ^ An abbreviation for PointerType t (AddrSpace 0)
half = FloatingPointType 16 IEEE -- ^ An abbreviation for FloatingPointType 16 IEEE
float = FloatingPointType 32 IEEE -- ^ An abbreviation for FloatingPointType 32 IEEE
double = FloatingPointType 64 IEEE -- ^ An abbreviation for FloatingPointType 64 IEEE
fp128 = FloatingPointType 128 IEEE -- ^ An abbreviation for FloatingPointType 128 IEEE
x86_fp80 = FloatingPointType 80 DoubleExtended -- ^ An abbreviation for FloatingPointType 80 DoubleExtended
ppc_fp128 = FloatingPointType 128 PairOfFloats -- ^ An abbreviation for FloatingPointType 128 PairOfFloats
