-- | A representation of an LLVM type
module LLVM.General.AST.Type where

import LLVM.General.AST.AddrSpace
import LLVM.General.AST.Name

import Data.Word (Word32, Word64)

-- | LLVM supports some special formats floating point format. This type is to distinguish those format.
-- I believe it's treated as a format for "a" float, as opposed to a vector of two floats, because
-- its intended usage is to represent a single number with a combined significand.
data FloatingPointFormat
  = IEEE
  | DoubleExtended
  | PairOfFloats
  deriving (Eq, Ord, Read, Show)

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
  deriving (Eq, Ord, Read, Show)
