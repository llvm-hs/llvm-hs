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

newtype ModuleSetHandle = ModuleSetHandle Word

foreign import ccall safe "LLVM_Hs_CompileLayer_dispose" disposeCompileLayer ::
  Ptr CompileLayer -> IO ()

foreign import ccall safe "LLVM_Hs_CompileLayer_addModuleSet" addModuleSet ::
  Ptr CompileLayer ->
  Ptr DataLayout ->
  Ptr (Ptr Module) ->
  CUInt ->
  Ptr LambdaResolver ->
  IO ModuleSetHandle

foreign import ccall safe "LLVM_Hs_CompileLayer_removeModuleSet" removeModuleSet ::
  Ptr CompileLayer -> ModuleSetHandle -> IO ()

foreign import ccall safe "LLVM_Hs_CompileLayer_findSymbol" findSymbol ::
  Ptr CompileLayer -> CString -> LLVMBool -> IO (Ptr JITSymbol)

foreign import ccall safe "LLVM_Hs_CompileLayer_findSymbolIn" findSymbolIn ::
  Ptr CompileLayer -> ModuleSetHandle -> CString -> LLVMBool -> IO (Ptr JITSymbol)
