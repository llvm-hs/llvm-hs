{-# LANGUAGE
  ForeignFunctionInterface,
  RankNTypes
  #-}
module LLVM.Internal.FFI.Attribute where

import LLVM.Prelude

import Foreign.C
import Foreign.Ptr

import LLVM.Internal.FFI.Context
import LLVM.Internal.FFI.LLVMCTypes

type Slot = CUInt
type IntValue = Word64

{-
Data model:
llvm::Attribute is one function or parameter attribute

llvm::AttributeSet stores a set of function, return or parameter attributes

llvm::AttributeList stores the AttributeSet for the function itself,
the return value and for the functions parameters.

Encode path:
Use AttrBuilder on the C++ side only, to implement [Attribute] -> AttributeList
AttributeLists -> whole AttributeList
-}

data MixedAttributeType
data FunctionAttributeType
data ParameterAttributeType
data AttributeImpl a
data AttributeSetImpl a
data AttributeListImpl

type Attribute a = Ptr (AttributeImpl a)
type FunctionAttribute = Attribute FunctionAttributeType
type ParameterAttribute = Attribute ParameterAttributeType
newtype AttributeIndex = AttributeIndex CUInt

type AttributeSet a = Ptr (AttributeSetImpl a)
-- type MixedAttributeSet = AttributeSet MixedAttributeType
type FunctionAttributeSet = AttributeSet FunctionAttributeType
type ParameterAttributeSet = AttributeSet ParameterAttributeType
type AttributeList = Ptr AttributeListImpl

forgetAttributeType :: AttributeSet a -> AttributeSet MixedAttributeType
forgetAttributeType = castPtr

functionIndex :: AttributeIndex
functionIndex = AttributeIndex (-1)
returnIndex :: AttributeIndex
returnIndex = AttributeIndex 0

data AttrSetDecoder a = AttrSetDecoder {
    attrSetDecoderAttributesAtIndex :: forall b. a -> AttributeIndex -> IO (AttributeSet b),
    attrSetDecoderCountParams :: a -> IO CUInt
  }

foreign import ccall unsafe "LLVM_Hs_AttributeKindAsEnum" parameterAttributeKindAsEnum ::
  ParameterAttribute -> IO ParameterAttributeKind

foreign import ccall unsafe "LLVM_Hs_AttributeKindAsEnum" functionAttributeKindAsEnum ::
  FunctionAttribute -> IO FunctionAttributeKind

foreign import ccall unsafe "LLVM_Hs_IsStringAttribute" isStringAttribute ::
  Attribute a -> IO LLVMBool

foreign import ccall unsafe "LLVM_Hs_AttributeKindAsString" attributeKindAsString ::
  Attribute a -> Ptr CSize -> IO (Ptr CChar)

foreign import ccall unsafe "LLVM_Hs_AttributeValueAsString" attributeValueAsString ::
  Attribute a -> Ptr CSize -> IO (Ptr CChar)

foreign import ccall unsafe "LLVM_Hs_AttributeValueAsInt" attributeValueAsInt ::
  Attribute a -> IO Word64

foreign import ccall unsafe "LLVM_Hs_getNumAttributes" getNumAttributes ::
  AttributeSet a -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_getAttributes" getAttributes ::
  AttributeSet a -> Ptr (Attribute a) -> IO ()

foreign import ccall unsafe "LLVM_Hs_GetAttributeList" getAttributeList ::
  Ptr Context -> AttributeIndex -> AttributeSet a -> IO AttributeList

foreign import ccall unsafe "LLVM_Hs_BuildAttributeList" buildAttributeList ::
  Ptr Context -> FunctionAttributeSet -> ParameterAttributeSet -> Ptr ParameterAttributeSet -> CUInt -> IO AttributeList

foreign import ccall unsafe "LLVM_Hs_DisposeAttributeList" disposeAttributeList ::
  AttributeList -> IO ()

foreign import ccall unsafe "LLVM_Hs_GetAttributeSet" getAttributeSet ::
  Ptr Context -> Ptr (AttrBuilder a) -> IO (AttributeSet a)

foreign import ccall unsafe "LLVM_Hs_DisposeAttributeSet" disposeAttributeSet ::
  AttributeSet a -> IO ()

foreign import ccall unsafe "LLVM_Hs_AttributeSetsEqual" attributeSetsEqual ::
  AttributeSet a -> AttributeSet a -> IO LLVMBool

foreign import ccall unsafe "LLVM_Hs_AttributeSetHasAttributes" attributeSetHasAttributes ::
  AttributeSet a -> IO LLVMBool

data AttrBuilder a
type FunctionAttrBuilder = AttrBuilder FunctionAttributeType
type ParameterAttrBuilder = AttrBuilder ParameterAttributeType

foreign import ccall unsafe "LLVM_Hs_GetAttrBuilderSize" getAttrBuilderSize ::
  CSize

foreign import ccall unsafe "LLVM_Hs_AttrBuilderFromAttrSet" attrBuilderFromSet ::
  AttributeSet a -> IO (Ptr (AttrBuilder a))

foreign import ccall unsafe "LLVM_Hs_DisposeAttrBuilder" disposeAttrBuilder ::
  Ptr (AttrBuilder a) -> IO ()

foreign import ccall unsafe "LLVM_Hs_AttrBuilderMerge" mergeAttrBuilder ::
  Ptr (AttrBuilder a) -> Ptr (AttrBuilder a) -> IO ()

foreign import ccall unsafe "LLVM_Hs_ConstructAttrBuilder" constructAttrBuilder ::
  Ptr Word8 -> IO (Ptr (AttrBuilder a))

foreign import ccall unsafe "LLVM_Hs_DestroyAttrBuilder" destroyAttrBuilder ::
  Ptr (AttrBuilder a) -> IO ()

foreign import ccall unsafe "LLVM_Hs_AttrBuilderAddAttributeKind" attrBuilderAddFunctionAttributeKind ::
  Ptr FunctionAttrBuilder -> FunctionAttributeKind -> IO ()

foreign import ccall unsafe "LLVM_Hs_AttrBuilderAddAttributeKind" attrBuilderAddParameterAttributeKind ::
  Ptr ParameterAttrBuilder -> ParameterAttributeKind -> IO ()

foreign import ccall unsafe "LLVM_Hs_AttrBuilderAddStringAttribute" attrBuilderAddStringAttribute ::
  Ptr (AttrBuilder a) -> Ptr CChar -> CSize -> Ptr CChar -> CSize -> IO ()

foreign import ccall unsafe "LLVM_Hs_AttrBuilderAddAlignment" attrBuilderAddAlignment ::
  Ptr ParameterAttrBuilder -> Word64 -> IO ()

foreign import ccall unsafe "LLVM_Hs_AttrBuilderAddStackAlignment" attrBuilderAddStackAlignment ::
  Ptr FunctionAttrBuilder -> Word64 -> IO ()

-- The CInt is 0 if the last value is null and 1 otherwise
foreign import ccall unsafe "LLVM_Hs_AttrBuilderAddAllocSize" attrBuilderAddAllocSize' ::
  Ptr FunctionAttrBuilder -> CUInt -> CUInt -> LLVMBool -> IO ()

attrBuilderAddAllocSize :: Ptr FunctionAttrBuilder -> CUInt -> (CUInt, LLVMBool) -> IO ()
attrBuilderAddAllocSize b i (y, isJust) = attrBuilderAddAllocSize' b i y isJust

foreign import ccall unsafe "LLVM_Hs_AttrBuilderAddDereferenceableAttr" attrBuilderAddDereferenceable ::
  Ptr ParameterAttrBuilder -> Word64 -> IO ()

foreign import ccall unsafe "LLVM_Hs_AttrBuilderAddDereferenceableOrNullAttr" attrBuilderAddDereferenceableOrNull ::
  Ptr ParameterAttrBuilder -> Word64 -> IO ()

foreign import ccall unsafe "LLVM_Hs_AttributeGetAllocSizeArgs" attributeGetAllocSizeArgs ::
  FunctionAttribute -> Ptr CUInt -> Ptr CUInt -> IO LLVMBool
