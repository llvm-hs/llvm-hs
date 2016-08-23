{-# LANGUAGE
  ForeignFunctionInterface,
  MultiParamTypeClasses,
  UndecidableInstances
  #-}
-- | FFI functions for handling the LLVM GlobalValue class
module LLVM.General.Internal.FFI.GlobalValue where

import LLVM.General.Prelude

import Foreign.Ptr
import Foreign.C

import LLVM.General.Internal.FFI.PtrHierarchy
import LLVM.General.Internal.FFI.LLVMCTypes

data COMDAT

foreign import ccall unsafe "LLVMIsAGlobalValue" isAGlobalValue ::
  Ptr Value -> IO (Ptr GlobalValue)

foreign import ccall unsafe "LLVMGetLinkage" getLinkage ::
  Ptr GlobalValue -> IO Linkage

foreign import ccall unsafe "LLVMSetLinkage" setLinkage ::
  Ptr GlobalValue -> Linkage -> IO ()

foreign import ccall unsafe "LLVMGetSection" getSection ::
  Ptr GlobalValue -> IO CString

foreign import ccall unsafe "LLVMSetSection" setSection ::
  Ptr GlobalValue -> CString -> IO ()

foreign import ccall unsafe "LLVM_General_GetCOMDAT" getCOMDAT ::
  Ptr GlobalValue -> IO (Ptr COMDAT)

foreign import ccall unsafe "LLVM_General_SetCOMDAT" setCOMDAT ::
  Ptr GlobalObject -> Ptr COMDAT -> IO ()

foreign import ccall unsafe "LLVM_General_GetCOMDATName" getCOMDATName ::
  Ptr COMDAT -> Ptr CSize -> IO (Ptr CChar)

foreign import ccall unsafe "LLVM_General_GetCOMDATSelectionKind" getCOMDATSelectionKind ::
  Ptr COMDAT -> IO COMDATSelectionKind

foreign import ccall unsafe "LLVM_General_SetCOMDATSelectionKind" setCOMDATSelectionKind ::
  Ptr COMDAT -> COMDATSelectionKind -> IO ()

foreign import ccall unsafe "LLVMGetVisibility" getVisibility ::
  Ptr GlobalValue -> IO Visibility

foreign import ccall unsafe "LLVMSetVisibility" setVisibility ::
  Ptr GlobalValue -> Visibility -> IO ()

foreign import ccall unsafe "LLVMGetDLLStorageClass" getDLLStorageClass ::
  Ptr GlobalValue -> IO DLLStorageClass

foreign import ccall unsafe "LLVMSetDLLStorageClass" setDLLStorageClass ::
  Ptr GlobalValue -> DLLStorageClass -> IO ()

foreign import ccall unsafe "LLVMGetAlignment" getAlignment ::
  Ptr GlobalValue -> IO CUInt

foreign import ccall unsafe "LLVMSetAlignment" setAlignment ::
  Ptr GlobalValue -> CUInt -> IO ()

foreign import ccall unsafe "LLVM_General_GetUnnamedAddr" getUnnamedAddr ::
  Ptr GlobalValue -> IO UnnamedAddr

foreign import ccall unsafe "LLVM_General_SetUnnamedAddr" setUnnamedAddr ::
  Ptr GlobalValue -> UnnamedAddr -> IO ()

foreign import ccall unsafe "LLVM_General_GetThreadLocalMode" getThreadLocalMode ::
  Ptr GlobalValue -> IO ThreadLocalMode

foreign import ccall unsafe "LLVM_General_SetThreadLocalMode" setThreadLocalMode ::
  Ptr GlobalValue -> ThreadLocalMode -> IO ()


