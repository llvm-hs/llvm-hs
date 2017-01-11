{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.Internal.FFI.OrcJIT.IRCompileLayer where

import LLVM.Prelude

import LLVM.Internal.FFI.DataLayout
import LLVM.Internal.FFI.LLVMCTypes
import LLVM.Internal.FFI.Module
import LLVM.Internal.FFI.OrcJIT
import LLVM.Internal.FFI.Target

import Foreign.Ptr
import Foreign.C

data IRCompileLayer
data ModuleSetHandle

foreign import ccall safe "LLVM_Hs_createIRCompileLayer" createIRCompileLayer ::
  Ptr ObjectLinkingLayer -> Ptr TargetMachine -> IO (Ptr IRCompileLayer)

foreign import ccall safe "LLVM_Hs_disposeIRCompileLayer" disposeIRCompileLayer ::
  Ptr IRCompileLayer -> IO ()

foreign import ccall safe "LLVM_Hs_IRCompileLayer_addModuleSet" addModuleSet ::
  Ptr IRCompileLayer ->
  Ptr DataLayout ->
  Ptr (Ptr Module) ->
  CUInt ->
  Ptr LambdaResolver ->
  IO (Ptr ModuleSetHandle)

foreign import ccall safe "LLVM_Hs_IRCompileLayer_removeModuleSet" removeModuleSet ::
  Ptr IRCompileLayer -> Ptr ModuleSetHandle -> IO ()

foreign import ccall safe "LLVM_Hs_IRCompileLayer_findSymbol" findSymbol ::
  Ptr IRCompileLayer -> CString -> LLVMBool -> IO (Ptr JITSymbol)
