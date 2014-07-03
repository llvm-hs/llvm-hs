{-# LANGUAGE
  ForeignFunctionInterface
  #-}
module LLVM.General.Internal.FFI.Threading where

import Foreign.C

import LLVM.General.Internal.FFI.LLVMCTypes
import LLVM.General.Internal.FFI.Module

foreign import ccall unsafe "LLVMStartMultithreaded" startMultithreaded ::
  IO LLVMBool

foreign import ccall unsafe "LLVMStopMultithreaded" stopMultithreaded ::
  IO ()

foreign import ccall unsafe "LLVMIsMultithreaded" isMultithreaded ::
  IO LLVMBool
