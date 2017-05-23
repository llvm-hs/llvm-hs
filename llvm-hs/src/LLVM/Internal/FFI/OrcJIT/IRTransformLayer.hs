{-# LANGUAGE ForeignFunctionInterface, MultiParamTypeClasses #-}
module LLVM.Internal.FFI.OrcJIT.IRTransformLayer where

import LLVM.Prelude

import LLVM.Internal.FFI.OrcJIT.CompileLayer
import LLVM.Internal.FFI.PtrHierarchy
import LLVM.Internal.FFI.Module

import Foreign.Ptr

data IRTransformLayer
instance ChildOf CompileLayer IRTransformLayer

type ModuleTransform = Ptr Module -> IO (Ptr Module)

foreign import ccall "wrapper" wrapModuleTransform ::
  ModuleTransform -> IO (FunPtr ModuleTransform)

foreign import ccall safe "LLVM_Hs_createIRTransformLayer" createIRTransformLayer ::
  Ptr CompileLayer -> FunPtr ModuleTransform -> IO (Ptr IRTransformLayer)
