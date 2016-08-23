{-# LANGUAGE
  ForeignFunctionInterface,
  MultiParamTypeClasses
  #-}

module LLVM.General.Internal.FFI.Function where

import LLVM.General.Prelude

import Foreign.Ptr
import Foreign.C

import LLVM.General.Internal.FFI.Attribute
import LLVM.General.Internal.FFI.Context
import LLVM.General.Internal.FFI.LLVMCTypes
import LLVM.General.Internal.FFI.PtrHierarchy

foreign import ccall unsafe "LLVM_General_GetFunctionCallingConvention" getFunctionCallingConvention ::
  Ptr Function -> IO CallingConvention

foreign import ccall unsafe "LLVM_General_SetFunctionCallingConvention" setFunctionCallingConvention ::
  Ptr Function -> CallingConvention -> IO ()

foreign import ccall unsafe "LLVM_General_GetFunctionMixedAttributeSet" getMixedAttributeSet ::
  Ptr Function -> IO MixedAttributeSet

foreign import ccall unsafe "LLVM_General_SetFunctionMixedAttributeSet" setMixedAttributeSet ::
  Ptr Function -> MixedAttributeSet -> IO ()

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


foreign import ccall unsafe "LLVM_General_HasFunctionPrefixData" hasPrefixData ::
  Ptr Function -> IO LLVMBool

foreign import ccall unsafe "LLVM_General_GetFunctionPrefixData" getPrefixData ::
  Ptr Function -> IO (Ptr Constant)

foreign import ccall unsafe "LLVM_General_SetFunctionPrefixData" setPrefixData ::
  Ptr Function -> Ptr Constant -> IO ()

foreign import ccall unsafe "LLVMHasPersonalityFn" hasPersonalityFn ::
  Ptr Function -> IO LLVMBool

foreign import ccall unsafe "LLVM_General_SetPersonalityFn" setPersonalityFn ::
  Ptr Function -> Ptr Constant -> IO ()

foreign import ccall unsafe "LLVMGetPersonalityFn" getPersonalityFn ::
  Ptr Function -> IO (Ptr Constant)
