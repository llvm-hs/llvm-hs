{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.General.Internal.FFI.OrcJIT.CompileOnDemandLayer where

import LLVM.General.Prelude

import Foreign.C
import Foreign.Ptr

import LLVM.General.Internal.FFI.DataLayout
import LLVM.General.Internal.FFI.LLVMCTypes
import LLVM.General.Internal.FFI.Module
import LLVM.General.Internal.FFI.OrcJIT
import LLVM.General.Internal.FFI.OrcJIT.IRCompileLayer (IRCompileLayer)
import LLVM.General.Internal.FFI.PtrHierarchy

data IndirectStubsManagerBuilder
data JITCompileCallbackManager
data CompileOnDemandLayer
data Set a
data ModuleSetHandle

type PartitioningFn = Ptr Function -> Ptr (Set (Ptr Function)) -> IO ()

foreign import ccall "wrapper" wrapPartitioningFn ::
  PartitioningFn -> IO (FunPtr PartitioningFn)

foreign import ccall "wrapper" wrapErrorHandler ::
  IO () -> IO (FunPtr (IO ()))

foreign import ccall safe "LLVM_General_createLocalCompileCallbackManager" createLocalCompileCallbackManager ::
  CString -> TargetAddress -> IO (Ptr JITCompileCallbackManager)

foreign import ccall safe "LLVM_General_disposeCallbackManager" disposeCallbackManager ::
  Ptr JITCompileCallbackManager -> IO ()

foreign import ccall safe "LLVM_General_createLocalIndirectStubsManagerBuilder" createLocalIndirectStubsManagerBuilder ::
  CString -> IO (Ptr IndirectStubsManagerBuilder)

foreign import ccall safe "LLVM_General_disposeIndirectStubsManagerBuilder" disposeIndirectStubsManagerBuilder ::
  Ptr IndirectStubsManagerBuilder -> IO ()

foreign import ccall safe "LLVM_General_insertFun" insertFun ::
  Ptr (Set (Ptr Function)) -> Ptr Function -> IO ()

foreign import ccall safe "LLVM_General_createCompileOnDemandLayer" createCompileOnDemandLayer ::
  Ptr IRCompileLayer ->
  FunPtr PartitioningFn ->
  Ptr JITCompileCallbackManager ->
  Ptr IndirectStubsManagerBuilder ->
  LLVMBool ->
  IO (Ptr CompileOnDemandLayer)

foreign import ccall safe "LLVM_General_disposeCompileOnDemandLayer" disposeCompileOnDemandLayer ::
  Ptr CompileOnDemandLayer ->
  IO ()

foreign import ccall safe "LLVM_General_CompileOnDemandLayer_addModuleSet" addModuleSet ::
  Ptr CompileOnDemandLayer ->
  Ptr DataLayout ->
  Ptr (Ptr Module) ->
  CUInt ->
  Ptr LambdaResolver ->
  IO (Ptr ModuleSetHandle)

foreign import ccall safe "LLVM_General_CompileOnDemandLayer_removeModuleSet" removeModuleSet ::
  Ptr CompileOnDemandLayer ->
  Ptr ModuleSetHandle ->
  IO ()

foreign import ccall safe "LLVM_General_CompileOnDemandLayer_findSymbol" findSymbol ::
  Ptr CompileOnDemandLayer ->
  CString ->
  LLVMBool ->
  IO (Ptr JITSymbol)
