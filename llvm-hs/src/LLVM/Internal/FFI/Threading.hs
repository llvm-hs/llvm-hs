{-# LANGUAGE
  ForeignFunctionInterface
  #-}
module LLVM.Internal.FFI.Threading where

import LLVM.Prelude

import Foreign.C

import LLVM.Internal.FFI.LLVMCTypes

foreign import ccall unsafe "LLVMIsMultithreaded" isMultithreaded ::
  IO LLVMBool
