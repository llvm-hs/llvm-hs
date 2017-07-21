{-# LANGUAGE
  ForeignFunctionInterface,
  MultiParamTypeClasses
  #-}

module LLVM.Internal.FFI.Function where

import LLVM.Prelude

import Foreign.Ptr
import Foreign.C

import LLVM.Internal.FFI.Attribute
import LLVM.Internal.FFI.Context
import LLVM.Internal.FFI.LLVMCTypes
import LLVM.Internal.FFI.PtrHierarchy

foreign import ccall unsafe "LLVMGetFunctionCallConv" getFunctionCallingConvention ::
  Ptr Function -> IO CallingConvention

foreign import ccall unsafe "LLVMSetFunctionCallConv" setFunctionCallingConvention ::
  Ptr Function -> CallingConvention -> IO ()

foreign import ccall unsafe "LLVM_Hs_SetFunctionAttributeList" setAttributeList ::
  Ptr Function -> AttributeList -> IO ()

foreign import ccall unsafe "LLVM_Hs_FunctionAttributesAtIndex" attributesAtIndex ::
  Ptr Function -> AttributeIndex -> IO (AttributeSet b)

foreign import ccall unsafe "LLVMGetFirstBasicBlock" getFirstBasicBlock ::
  Ptr Function -> IO (Ptr BasicBlock)

foreign import ccall unsafe "LLVMGetLastBasicBlock" getLastBasicBlock ::
  Ptr Function -> IO (Ptr BasicBlock)

foreign import ccall unsafe "LLVMGetNextBasicBlock" getNextBasicBlock ::
  Ptr BasicBlock -> IO (Ptr BasicBlock)

foreign import ccall unsafe "LLVMAppendBasicBlockInContext" appendBasicBlockInContext ::
  Ptr Context -> Ptr Function -> CString -> IO (Ptr BasicBlock)


foreign import ccall unsafe "LLVMCountParams" countParams ::
  Ptr Function -> IO CUInt

foreign import ccall unsafe "LLVMGetParams" getParams ::
  Ptr Function -> Ptr (Ptr Parameter) -> IO ()

foreign import ccall unsafe "LLVMGetGC" getGC ::
  Ptr Function -> IO CString

foreign import ccall unsafe "LLVMSetGC" setGC ::
  Ptr Function -> CString -> IO ()


foreign import ccall unsafe "LLVM_Hs_HasFunctionPrefixData" hasPrefixData ::
  Ptr Function -> IO LLVMBool

foreign import ccall unsafe "LLVM_Hs_GetFunctionPrefixData" getPrefixData ::
  Ptr Function -> IO (Ptr Constant)

foreign import ccall unsafe "LLVM_Hs_SetFunctionPrefixData" setPrefixData ::
  Ptr Function -> Ptr Constant -> IO ()

foreign import ccall unsafe "LLVMHasPersonalityFn" hasPersonalityFn ::
  Ptr Function -> IO LLVMBool

foreign import ccall unsafe "LLVM_Hs_SetPersonalityFn" setPersonalityFn ::
  Ptr Function -> Ptr Constant -> IO ()

foreign import ccall unsafe "LLVMGetPersonalityFn" getPersonalityFn ::
  Ptr Function -> IO (Ptr Constant)
