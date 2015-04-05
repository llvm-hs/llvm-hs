{-# LANGUAGE
  ForeignFunctionInterface,
  MultiParamTypeClasses
  #-}

module LLVM.General.Internal.FFI.Function where

import LLVM.General.Prelude

import Foreign.Ptr
import Foreign.C

import LLVM.General.Internal.FFI.Context
import LLVM.General.Internal.FFI.LLVMCTypes
import LLVM.General.Internal.FFI.PtrHierarchy

foreign import ccall unsafe "LLVMGetFunctionCallConv" getFunctionCallConv ::
    Ptr Function -> IO CallConv

foreign import ccall unsafe "LLVMSetFunctionCallConv" setFunctionCallConv ::
    Ptr Function -> CallConv -> IO ()

foreign import ccall unsafe "LLVMAddFunctionAttr" addFunctionAttr ::
    Ptr Function -> FunctionAttr -> IO ()

foreign import ccall unsafe "LLVMGetFunctionAttr" getFunctionAttr ::
    Ptr Function -> IO FunctionAttr

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

foreign import ccall unsafe "LLVMGetAttribute" getAttribute ::
    Ptr Parameter -> IO ParamAttr

foreign import ccall unsafe "LLVMAddAttribute" addAttribute ::
    Ptr Parameter -> ParamAttr -> IO ()

foreign import ccall unsafe "LLVM_General_GetFunctionRetAttr" getFunctionRetAttr ::
    Ptr Function -> IO ParamAttr

foreign import ccall unsafe "LLVM_General_AddFunctionRetAttr" addFunctionRetAttr ::
    Ptr Function -> ParamAttr -> IO ()

foreign import ccall unsafe "LLVMGetGC" getGC ::
  Ptr Function -> IO CString

foreign import ccall unsafe "LLVMSetGC" setGC ::
  Ptr Function -> CString -> IO ()
