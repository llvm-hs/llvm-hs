{-# LANGUAGE ForeignFunctionInterface, MultiParamTypeClasses #-}
module LLVM.Internal.FFI.OrcJIT.IRCompileLayer where

import LLVM.Prelude

import LLVM.Internal.FFI.OrcJIT.LinkingLayer
import LLVM.Internal.FFI.OrcJIT.CompileLayer
import LLVM.Internal.FFI.PtrHierarchy
import LLVM.Internal.FFI.Target

import Foreign.Ptr

data IRCompileLayer
instance ChildOf CompileLayer IRCompileLayer

foreign import ccall safe "LLVM_Hs_createLegacyIRCompileLayer" createIRCompileLayer ::
  Ptr LinkingLayer -> Ptr TargetMachine -> IO (Ptr IRCompileLayer)
