{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.Internal.FFI.OrcJIT.CompileOnDemandLayer where

import LLVM.Prelude

import Foreign.C
import Foreign.Ptr

import LLVM.Internal.FFI.DataLayout
import LLVM.Internal.FFI.LLVMCTypes
import LLVM.Internal.FFI.Module
import LLVM.Internal.FFI.OrcJIT
import LLVM.Internal.FFI.OrcJIT.IRCompileLayer (IRCompileLayer)
import LLVM.Internal.FFI.PtrHierarchy

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

foreign import ccall safe "LLVM_Hs_createLocalCompileCallbackManager" createLocalCompileCallbackManager ::
  CString -> TargetAddress -> IO (Ptr JITCompileCallbackManager)

foreign import ccall safe "LLVM_Hs_disposeCallbackManager" disposeCallbackManager ::
  Ptr JITCompileCallbackManager -> IO ()

foreign import ccall safe "LLVM_Hs_createLocalIndirectStubsManagerBuilder" createLocalIndirectStubsManagerBuilder ::
  CString -> IO (Ptr IndirectStubsManagerBuilder)

foreign import ccall safe "LLVM_Hs_disposeIndirectStubsManagerBuilder" disposeIndirectStubsManagerBuilder ::
  Ptr IndirectStubsManagerBuilder -> IO ()

foreign import ccall safe "LLVM_Hs_insertFun" insertFun ::
  Ptr (Set (Ptr Function)) -> Ptr Function -> IO ()

foreign import ccall safe "LLVM_Hs_createCompileOnDemandLayer" createCompileOnDemandLayer ::
  Ptr IRCompileLayer ->
  FunPtr PartitioningFn ->
  Ptr JITCompileCallbackManager ->
  Ptr IndirectStubsManagerBuilder ->
  LLVMBool ->
  IO (Ptr CompileOnDemandLayer)

foreign import ccall safe "LLVM_Hs_disposeCompileOnDemandLayer" disposeCompileOnDemandLayer ::
  Ptr CompileOnDemandLayer ->
  IO ()

foreign import ccall safe "LLVM_Hs_CompileOnDemandLayer_addModuleSet" addModuleSet ::
  Ptr CompileOnDemandLayer ->
  Ptr DataLayout ->
  Ptr (Ptr Module) ->
  CUInt ->
  Ptr LambdaResolver ->
  IO (Ptr ModuleSetHandle)

foreign import ccall safe "LLVM_Hs_CompileOnDemandLayer_removeModuleSet" removeModuleSet ::
  Ptr CompileOnDemandLayer ->
  Ptr ModuleSetHandle ->
  IO ()

foreign import ccall safe "LLVM_Hs_CompileOnDemandLayer_findSymbol" findSymbol ::
  Ptr CompileOnDemandLayer ->
  CString ->
  LLVMBool ->
  IO (Ptr JITSymbol)
