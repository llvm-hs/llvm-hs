{-# LANGUAGE
  ForeignFunctionInterface
  #-}
module LLVM.General.Internal.FFI.Threading where

import LLVM.General.Prelude

import Foreign.C

import LLVM.General.Internal.FFI.LLVMCTypes

foreign import ccall unsafe "LLVMStartMultithreaded" startMultithreaded ::
  IO LLVMBool

foreign import ccall unsafe "LLVMStopMultithreaded" stopMultithreaded ::
  IO ()

foreign import ccall unsafe "LLVMIsMultithreaded" isMultithreaded ::
  IO LLVMBool
