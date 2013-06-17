module LLVM.General.AST.Type where

import LLVM.General.AST.AddrSpace
import LLVM.General.AST.Name

import Data.Word (Word32, Word64)

data Type
    = VoidType
    | IntegerType { typeBits :: Word32 }
    | PointerType { pointerReferent :: Type, pointerAddrSpace :: AddrSpace }
    | FloatingPointType { typeBits :: Word32 }
    | FunctionType { resultType :: Type, argumentTypes :: [Type], isVarArg :: Bool }
    | VectorType { nVectorElements :: Word32, elementType :: Type }
    | StructureType { isPacked :: Bool, elementTypes :: [Type] }
    | ArrayType { nArrayElements :: Word64, elementType :: Type }
    | NamedTypeReference Name
    | MetadataType -- only to be used as a parameter type for a few intrinsics
    deriving (Eq, Ord, Read, Show)
