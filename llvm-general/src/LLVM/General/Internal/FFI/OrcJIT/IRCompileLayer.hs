{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.General.Internal.FFI.OrcJIT.IRCompileLayer where

import LLVM.General.Prelude

import LLVM.General.Internal.FFI.DataLayout
import LLVM.General.Internal.FFI.LLVMCTypes
import LLVM.General.Internal.FFI.Module
import LLVM.General.Internal.FFI.OrcJIT
import LLVM.General.Internal.FFI.Target

import Foreign.Ptr
import Foreign.C

data IRCompileLayer
data ModuleSetHandle

foreign import ccall safe "LLVM_General_createIRCompileLayer" createIRCompileLayer ::
  Ptr ObjectLinkingLayer -> Ptr TargetMachine -> IO (Ptr IRCompileLayer)

foreign import ccall safe "LLVM_General_disposeIRCompileLayer" disposeIRCompileLayer ::
  Ptr IRCompileLayer -> IO ()

foreign import ccall safe "LLVM_General_IRCompileLayer_addModuleSet" addModuleSet ::
  Ptr IRCompileLayer ->
  Ptr DataLayout ->
  Ptr (Ptr Module) ->
  CUInt ->
  Ptr LambdaResolver ->
  IO (Ptr ModuleSetHandle)

foreign import ccall safe "LLVM_General_IRCompileLayer_removeModuleSet" removeModuleSet ::
  Ptr IRCompileLayer -> Ptr ModuleSetHandle -> IO ()

foreign import ccall safe "LLVM_General_IRCompileLayer_findSymbol" findSymbol ::
  Ptr IRCompileLayer -> CString -> LLVMBool -> IO (Ptr JITSymbol)
