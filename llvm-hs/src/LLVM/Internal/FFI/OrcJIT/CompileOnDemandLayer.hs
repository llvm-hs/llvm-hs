{-# LANGUAGE ForeignFunctionInterface, MultiParamTypeClasses #-}
module LLVM.Internal.FFI.OrcJIT.CompileOnDemandLayer where

import LLVM.Prelude

import Foreign.C
import Foreign.Ptr

import LLVM.Internal.FFI.LLVMCTypes
import LLVM.Internal.FFI.OrcJIT
import LLVM.Internal.FFI.OrcJIT.CompileLayer
import LLVM.Internal.FFI.PtrHierarchy

data IndirectStubsManagerBuilder
data JITCompileCallbackManager
data Set a
data CompileOnDemandLayer
instance ChildOf CompileLayer CompileOnDemandLayer

type PartitioningFn = Ptr Function -> Ptr (Set (Ptr Function)) -> IO ()

foreign import ccall "wrapper" wrapPartitioningFn ::
  PartitioningFn -> IO (FunPtr PartitioningFn)

foreign import ccall "wrapper" wrapErrorHandler ::
  IO () -> IO (FunPtr (IO ()))

foreign import ccall safe "LLVM_Hs_createLocalCompileCallbackManager" createLocalCompileCallbackManager ::
  Ptr ExecutionSession -> CString -> TargetAddress -> IO (Ptr JITCompileCallbackManager)

foreign import ccall safe "LLVM_Hs_disposeCallbackManager" disposeCallbackManager ::
  Ptr JITCompileCallbackManager -> IO ()

foreign import ccall safe "LLVM_Hs_createLocalIndirectStubsManagerBuilder" createLocalIndirectStubsManagerBuilder ::
  CString -> IO (Ptr IndirectStubsManagerBuilder)

foreign import ccall safe "LLVM_Hs_disposeIndirectStubsManagerBuilder" disposeIndirectStubsManagerBuilder ::
  Ptr IndirectStubsManagerBuilder -> IO ()

foreign import ccall safe "LLVM_Hs_insertFun" insertFun ::
  Ptr (Set (Ptr Function)) -> Ptr Function -> IO ()

foreign import ccall safe "LLVM_Hs_createCompileOnDemandLayer" createCompileOnDemandLayer ::
  Ptr ExecutionSession ->
  Ptr CompileLayer ->
  FunPtr (ModuleKey -> IO (Ptr SymbolResolver)) ->
  FunPtr (ModuleKey -> Ptr SymbolResolver -> IO ()) ->
  FunPtr PartitioningFn ->
  Ptr JITCompileCallbackManager ->
  Ptr IndirectStubsManagerBuilder ->
  LLVMBool ->
  IO (Ptr CompileOnDemandLayer)
