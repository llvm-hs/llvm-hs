{-# LANGUAGE
  ForeignFunctionInterface,
  MultiParamTypeClasses,
  UndecidableInstances
  #-}
-- | FFI functions for handling the LLVM GlobalValue class
module LLVM.Internal.FFI.GlobalValue where

import LLVM.Prelude

import Foreign.Ptr
import Foreign.C

import LLVM.Internal.FFI.PtrHierarchy
import LLVM.Internal.FFI.LLVMCTypes

data COMDAT

foreign import ccall unsafe "LLVMIsAGlobalValue" isAGlobalValue ::
  Ptr Value -> IO (Ptr GlobalValue)

foreign import ccall unsafe "LLVMGetLinkage" getLinkage ::
  Ptr GlobalValue -> IO Linkage

foreign import ccall unsafe "LLVMSetLinkage" setLinkage ::
  Ptr GlobalValue -> Linkage -> IO ()

foreign import ccall unsafe "LLVM_Hs_GetSection" getSection ::
  Ptr GlobalValue -> Ptr CSize -> IO CString

foreign import ccall unsafe "LLVMSetSection" setSection ::
  Ptr GlobalValue -> CString -> IO ()

foreign import ccall unsafe "LLVM_Hs_GetCOMDAT" getCOMDAT ::
  Ptr GlobalValue -> IO (Ptr COMDAT)

foreign import ccall unsafe "LLVM_Hs_SetCOMDAT" setCOMDAT ::
  Ptr GlobalObject -> Ptr COMDAT -> IO ()

foreign import ccall unsafe "LLVM_Hs_GetCOMDATName" getCOMDATName ::
  Ptr COMDAT -> Ptr CSize -> IO (Ptr CChar)

foreign import ccall unsafe "LLVM_Hs_GetCOMDATSelectionKind" getCOMDATSelectionKind ::
  Ptr COMDAT -> IO COMDATSelectionKind

foreign import ccall unsafe "LLVM_Hs_SetCOMDATSelectionKind" setCOMDATSelectionKind ::
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

foreign import ccall unsafe "LLVMGetUnnamedAddress" getUnnamedAddr ::
  Ptr GlobalValue -> IO UnnamedAddr

foreign import ccall unsafe "LLVMSetUnnamedAddress" setUnnamedAddr ::
  Ptr GlobalValue -> UnnamedAddr -> IO ()

foreign import ccall unsafe "LLVM_Hs_GetThreadLocalMode" getThreadLocalMode ::
  Ptr GlobalValue -> IO ThreadLocalMode

foreign import ccall unsafe "LLVM_Hs_SetThreadLocalMode" setThreadLocalMode ::
  Ptr GlobalValue -> ThreadLocalMode -> IO ()

foreign import ccall unsafe "LLVM_Hs_GlobalObject_GetNumMetadata" getNumMetadata ::
  Ptr GlobalObject -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_GlobalObject_GetAllMetadata" getAllMetadata ::
  Ptr GlobalObject -> Ptr MDKindID -> Ptr (Ptr MDNode) -> IO ()

foreign import ccall unsafe "LLVM_Hs_GlobalObject_SetMetadata" setMetadata ::
  Ptr GlobalObject -> MDKindID -> Ptr MDNode -> IO ()
