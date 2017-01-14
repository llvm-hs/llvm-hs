{-# LANGUAGE
  ForeignFunctionInterface
  #-}
module LLVM.Internal.FFI.Attribute where

import LLVM.Prelude

import Foreign.C
import Foreign.Ptr

import LLVM.Internal.FFI.Context
import LLVM.Internal.FFI.LLVMCTypes

type Index = CInt
type Slot = CUInt
type IntValue = Word64

{-
Data model:
llvm::Attribute is one function or parameter attribute

llvm::AttributeSet is a mess.
It's used to represent, at different times:
a) the set of parameter attributes on a parameter
b) the set of parameter attributes for a functions return value
c) the set of function attributes for a function
d) All of the above

It is only possible to enumerate the attributes in an attribute set
given a "slot".

Encode path:
Use AttrBuilder on the C++ side only, to implement [Attribute] -> AttributeSet
AttributeSets -> whole AttributeSet

Decode strategy:
Store maps of AttributeSetImpl (Mess | Parameter | Function),
keyed by raw pointer. Expect Parameter and Function AttributeSetImpls
to have only one slot. Use the per-slot iterators to decode them
-}

data MixedAttributeType
data FunctionAttributeType
data ParameterAttributeType
data AttributeImpl a
data AttributeSetImpl a

type Attribute a = Ptr (AttributeImpl a)
type FunctionAttribute = Attribute FunctionAttributeType
type ParameterAttribute = Attribute ParameterAttributeType

type AttributeSet a = Ptr (AttributeSetImpl a)
type MixedAttributeSet = AttributeSet MixedAttributeType
type FunctionAttributeSet = AttributeSet FunctionAttributeType
type ParameterAttributeSet = AttributeSet ParameterAttributeType

forgetAttributeType :: AttributeSet a -> AttributeSet MixedAttributeType
forgetAttributeType = castPtr

functionIndex :: Index
functionIndex = -1
returnIndex :: Index
returnIndex = 0

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

foreign import ccall unsafe "LLVM_Hs_AttributeSetNumSlots" attributeSetNumSlots ::
  AttributeSet a -> IO Slot

foreign import ccall unsafe "LLVM_Hs_AttributeSetSlotIndex" attributeSetSlotIndex ::
  AttributeSet a -> Slot -> IO Index

foreign import ccall unsafe "LLVM_Hs_AttributeSetSlotAttributes" attributeSetSlotAttributes ::
  MixedAttributeSet -> Slot -> IO (AttributeSet a)

foreign import ccall unsafe "LLVM_Hs_AttributeSetGetAttributes" attributeSetGetAttributes ::
  AttributeSet a -> Slot -> Ptr CUInt -> IO (Ptr (Attribute a))

foreign import ccall unsafe "LLVM_Hs_GetAttributeSet" getAttributeSet ::
  Ptr Context -> Index -> Ptr (AttrBuilder a) -> IO (AttributeSet a)

foreign import ccall unsafe "LLVM_Hs_MixAttributeSets" mixAttributeSets ::
  Ptr Context -> Ptr MixedAttributeSet -> CUInt -> IO MixedAttributeSet

data AttrBuilder a
type FunctionAttrBuilder = AttrBuilder FunctionAttributeType
type ParameterAttrBuilder = AttrBuilder ParameterAttributeType

foreign import ccall unsafe "LLVM_Hs_GetAttrBuilderSize" getAttrBuilderSize ::
  CSize

foreign import ccall unsafe "LLVM_Hs_ConstructAttrBuilder" constructAttrBuilder ::
  Ptr Word8 -> IO (Ptr (AttrBuilder a))

foreign import ccall unsafe "LLVM_Hs_DestroyAttrBuilder" destroyAttrBuilder ::
  Ptr (AttrBuilder a) -> IO ()

foreign import ccall unsafe "LLVM_Hs_AttrBuilderAddAttributeKind" attrBuilderAddFunctionAttributeKind ::
  Ptr FunctionAttrBuilder -> FunctionAttributeKind -> IO ()

foreign import ccall unsafe "LLVM_Hs_AttrBuilderAddAttributeKind" attrBuilderAddParameterAttributeKind ::
  Ptr ParameterAttrBuilder -> ParameterAttributeKind -> IO ()

foreign import ccall unsafe "LLVM_Hs_AttrBuilderAddStringAttribute" attrBuilderAddStringAttribute ::
  Ptr FunctionAttrBuilder -> Ptr CChar -> CSize -> Ptr CChar -> CSize -> IO ()

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
