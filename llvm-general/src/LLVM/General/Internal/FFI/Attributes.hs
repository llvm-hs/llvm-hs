{-# LANGUAGE
  ForeignFunctionInterface
  #-}
module LLVM.General.Internal.FFI.Attributes where

import LLVM.General.Prelude

import Foreign.Ptr
import Foreign.C

import LLVM.General.Internal.FFI.LLVMCTypes

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

functionIndex :: Index
functionIndex = -1
returnIndex :: Index
returnIndex = -1

{-
data AttrBuilder a
foreign import ccall unsafe "LLVM_General_CreateAttrBuilder" createAttrBuilder ::
  IO (Ptr (AttrBuilder a))

foreign import ccall unsafe "LLVM_General_DisposeAttrBuilder" disposeAttrBuilder ::
  Ptr (AttrBuilder a) -> IO ()

foreign import ccall unsafe "LLVM_General_AttrBuilderAdd" attrBuilderAdd ::
  Ptr (AttrBuilder a) -> AttrKind -> IntValue -> IO ()

data (AttributeSet a)
foreign import ccall unsafe "LLVM_General_CreateAttributeSet" createAttributeSet ::
  Ptr (AttrBuilder a) -> IO (Ptr (AttributeSet a))

foreign import ccall unsafe "LLVM_General_DisposeAttributeSet" disposeAttributeSet ::
  Ptr (AttributeSet a) -> IO ()
-}

foreign import ccall unsafe "LLVM_General_AttributeEnum" parameterAttributeEnum ::
  ParameterAttribute -> IO ParameterAttributeKind

foreign import ccall unsafe "LLVM_General_AttributeEnum" functionAttributeEnum ::
  FunctionAttribute -> IO FunctionAttributeKind

foreign import ccall unsafe "LLVM_General_AttributeValueAsInt" attributeValueAsInt ::
  Attribute a -> IO Word64

foreign import ccall unsafe "LLVM_General_AttributeSetNumSlots" attributeSetNumSlots ::
  AttributeSet a -> IO Slot

foreign import ccall unsafe "LLVM_General_AttributeSetSlotIndex" attributeSetSlotIndex ::
  AttributeSet a -> Slot -> IO Index

foreign import ccall unsafe "LLVM_General_AttributeSetSlotAttributes" attributeSetSlotAttributes ::
  MixedAttributeSet -> Slot -> IO (AttributeSet a)

foreign import ccall unsafe "LLVM_General_AttributeSetGetAttributes" attributeSetGetAttributes ::
  AttributeSet a -> Slot -> Ptr CUInt -> IO (Ptr (Attribute a))

{-
foreign import ccall unsafe "LLVM_General_AttributeKind" attributeKind ::
  Ptr (Attribute a) -> Index -> IO AttrKind

foreign import ccall unsafe "LLVM_General_AttributeSetIndexValueAsInt" attributeSetIndexValueAsInt ::
  Ptr (AttributeSet a) -> Index -> IO IntValue

foreign import ccall unsafe "LLVM_General_AttributeSetIndexValueAsString" attributeSetIndexValueAsString ::
  Ptr (AttributeSet a) -> Index -> Ptr CUInt -> IO (Ptr CChar)

foreign import ccall unsafe "LLVM_General_AttributeSetIndexKindAsString" attributeSetIndexValueAsString ::
  Ptr (AttributeSet a) -> Index -> Ptr CUInt -> IO (Ptr CChar)
-}



  
