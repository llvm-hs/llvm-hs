-- | A representation of an LLVM type
module LLVM.General.AST.Type where

import LLVM.General.Prelude

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

-- | An abbreviation for 'VoidType'
void :: Type
void = VoidType

-- | An abbreviation for 'IntegerType' 1
i1 :: Type
i1 = IntegerType 1

-- | An abbreviation for 'IntegerType' 8
i8 :: Type
i8 = IntegerType 8

-- | An abbreviation for 'IntegerType' 16
i16 :: Type
i16 = IntegerType 16

-- | An abbreviation for 'IntegerType' 32
i32 :: Type
i32 = IntegerType 32

-- | An abbreviation for 'IntegerType' 64
i64 :: Type
i64 = IntegerType 64

-- | An abbreviation for 'IntegerType' 128
i128 :: Type
i128 = IntegerType 128

-- | An abbreviation for 'PointerType' t ('AddrSpace' 0)
ptr :: Type -> Type
ptr t = PointerType t (AddrSpace 0)

-- | An abbreviation for 'FloatingPointType' 16 'IEEE'
half :: Type
half = FloatingPointType 16 IEEE

-- | An abbreviation for 'FloatingPointType' 32 'IEEE'
float :: Type
float = FloatingPointType 32 IEEE

-- | An abbreviation for 'FloatingPointType' 64 'IEEE'
double :: Type
double = FloatingPointType 64 IEEE

-- | An abbreviation for 'FloatingPointType' 128 'IEEE'
fp128 :: Type
fp128 = FloatingPointType 128 IEEE

-- | An abbreviation for 'FloatingPointType' 80 'DoubleExtended'
x86_fp80 :: Type
x86_fp80 = FloatingPointType 80 DoubleExtended

-- | An abbreviation for 'FloatingPointType' 128 'PairOfFloats'
ppc_fp128 :: Type
ppc_fp128 = FloatingPointType 128 PairOfFloats

