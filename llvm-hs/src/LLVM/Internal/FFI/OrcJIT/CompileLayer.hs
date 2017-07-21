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

-- | Abstract type representing a module in a 'LLVM.OrcJIT.CompileLayer'.
newtype ModuleHandle = ModuleHandle Word

foreign import ccall safe "LLVM_Hs_CompileLayer_dispose" disposeCompileLayer ::
  Ptr CompileLayer -> IO ()

foreign import ccall safe "LLVM_Hs_CompileLayer_addModule" addModule ::
  Ptr CompileLayer ->
  Ptr DataLayout ->
  Ptr Module ->
  Ptr LambdaResolver ->
  Ptr (OwnerTransfered CString) ->
  IO ModuleHandle

foreign import ccall safe "LLVM_Hs_CompileLayer_removeModule" removeModule ::
  Ptr CompileLayer -> ModuleHandle -> IO ()

foreign import ccall safe "LLVM_Hs_CompileLayer_findSymbol" findSymbol ::
  Ptr CompileLayer -> CString -> LLVMBool -> IO (Ptr JITSymbol)

foreign import ccall safe "LLVM_Hs_CompileLayer_findSymbolIn" findSymbolIn ::
  Ptr CompileLayer -> ModuleHandle -> CString -> LLVMBool -> IO (Ptr JITSymbol)
