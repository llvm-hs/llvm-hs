{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.Internal.FFI.OrcJIT.CompileLayer where

import LLVM.Prelude

import LLVM.Internal.FFI.DataLayout
import LLVM.Internal.FFI.LLVMCTypes
import LLVM.Internal.FFI.Module
import LLVM.Internal.FFI.OrcJIT

import Foreign.C
import Foreign.Ptr

data CompileLayer

foreign import ccall safe "LLVM_Hs_CompileLayer_dispose" disposeCompileLayer ::
  Ptr CompileLayer -> IO ()

foreign import ccall safe "LLVM_Hs_CompileLayer_addModule" addModule ::
  Ptr CompileLayer ->
  Ptr DataLayout ->
  ModuleKey ->
  Ptr Module ->
  Ptr (OwnerTransfered CString) ->
  IO ()

foreign import ccall safe "LLVM_Hs_CompileLayer_removeModule" removeModule ::
  Ptr CompileLayer -> ModuleKey -> IO ()

foreign import ccall safe "LLVM_Hs_CompileLayer_findSymbol" findSymbol ::
  Ptr CompileLayer -> CString -> LLVMBool -> IO (Ptr JITSymbol)

foreign import ccall safe "LLVM_Hs_CompileLayer_findSymbolIn" findSymbolIn ::
  Ptr CompileLayer -> ModuleKey -> CString -> LLVMBool -> IO (Ptr JITSymbol)
