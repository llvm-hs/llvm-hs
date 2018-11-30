-- | A representation of an LLVM type
module LLVM.AST.Type where

import LLVM.Prelude

import LLVM.AST.AddrSpace
import LLVM.AST.Name

-- | LLVM supports some special formats floating point format. This type is to distinguish those format. Also see  <http://llvm.org/docs/LangRef.html#floating-point-types>
data FloatingPointType
  = HalfFP      -- ^ 16-bit floating point value
  | FloatFP     -- ^ 32-bit floating point value
  | DoubleFP    -- ^ 64-bit floating point value
  | FP128FP     -- ^ 128-bit floating point value (112-bit mantissa)
  | X86_FP80FP  -- ^ 80-bit floating point value (X87)
  | PPC_FP128FP -- ^ 128-bit floating point value (two 64-bits)
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <http://llvm.org/docs/LangRef.html#type-system>
data Type
  -- | <http://llvm.org/docs/LangRef.html#void-type>
  = VoidType
  -- | <http://llvm.org/docs/LangRef.html#integer-type>
  | IntegerType { typeBits :: Word32 }
  -- | <http://llvm.org/docs/LangRef.html#pointer-type>
  | PointerType { pointerReferent :: Type, pointerAddrSpace :: AddrSpace }
  -- | <http://llvm.org/docs/LangRef.html#floating-point-types>
  | FloatingPointType { floatingPointType :: FloatingPointType }
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
  -- | <http://llvm.org/docs/LangRef.html#label-type>
  | LabelType -- only to be used as the type of block names
  -- | <http://llvm.org/docs/LangRef.html#token-type>
  | TokenType
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

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

-- | An abbreviation for 'FloatingPointType' 'HalfFP'
half :: Type
half = FloatingPointType HalfFP

-- | An abbreviation for 'FloatingPointType' 'FloatFP'
float :: Type
float = FloatingPointType FloatFP

-- | An abbreviation for 'FloatingPointType' 'DoubleFP'
double :: Type
double = FloatingPointType DoubleFP

-- | An abbreviation for 'FloatingPointType' 'FP128FP'
fp128 :: Type
fp128 = FloatingPointType FP128FP

-- | An abbreviation for 'FloatingPointType' 'X86_FP80FP'
x86_fp80 :: Type
x86_fp80 = FloatingPointType X86_FP80FP

-- | An abbreviation for 'FloatingPointType' 'PPC_FP128FP'
ppc_fp128 :: Type
ppc_fp128 = FloatingPointType PPC_FP128FP

