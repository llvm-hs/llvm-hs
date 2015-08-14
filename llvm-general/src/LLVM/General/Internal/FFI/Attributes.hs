{-# LANGUAGE
  ForeignFunctionInterface
  #-}

module LLVM.General.Internal.FFI.Attributes where

import Data.Word
import Foreign.Ptr
import Foreign.C

type Index = CInt
type Slot = CUInt
type IntValue = Word64

{-
Data model:
llvm::Attribute is one function or parameter attribute

llvm::AttributeSet is some of the attributes for a function:
 function attributes and
 parameter attributes for each parameter
 parameter attributes for the return value
It's used (at least logically) hierarchically - one for each non-empty
such case and one for the whole, holding the others


Encode path:
AttrBuilder per field, (index, AttrBuilder) -> AttributeSet
AttributeSets -> whole AttributeSet

Decode path:
whole AttributeSet -> AttributeSet per field
decode AttributeSet into Attributes 
-}

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

foreign import ccall unsafe "LLVM_General_AttributeSetNumSlots" attributeSetNumSlots ::
  Ptr (AttributeSet a) -> IO Slot

foreign import ccall unsafe "LLVM_General_AttributeSetSlotIndex" attributeSetSlotIndex ::
  Ptr (AttributeSet a) -> Slot -> IO Index

foreign import ccall unsafe "LLVM_General_AttributeSetSlotLength" attributeSetSlotLength ::
  Ptr (AttributeSet a) -> Slot -> IO CUInt

foreign import ccall unsafe "LLVM_General_AttributeSetSlotLength" attributeSetGetSlotAttribute ::
  Ptr (AttributeSet a) -> Slot -> CUInt -> IO (Ptr Attribute)

foreign import ccall unsafe "LLVM_General_AttributeKind" attributeKind ::
  Ptr (Attribute a) -> Index -> IO AttrKind

foreign import ccall unsafe "LLVM_General_AttributeSetIndexValueAsInt" attributeSetIndexValueAsInt ::
  Ptr (AttributeSet a) -> Index -> IO IntValue

foreign import ccall unsafe "LLVM_General_AttributeSetIndexValueAsString" attributeSetIndexValueAsString ::
  Ptr (AttributeSet a) -> Index -> Ptr CUInt -> IO (Ptr CChar)

foreign import ccall unsafe "LLVM_General_AttributeSetIndexKindAsString" attributeSetIndexValueAsString ::
  Ptr (AttributeSet a) -> Index -> Ptr CUInt -> IO (Ptr CChar)
-}

data FixFunctionAttr
data FixParamAttr



  
