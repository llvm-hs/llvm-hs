{-# LANGUAGE ForeignFunctionInterface #-}

-- | FFI functions for handling the DynamicLibrary class
module LLVM.Internal.FFI.DynamicLibrary where

import LLVM.Prelude

import Foreign.C

import LLVM.Internal.FFI.LLVMCTypes

-- | <https://llvm.org/doxygen/Support_8h.html#a6fc1331c1a6d2cc6f0fda94f4a4636f9>
foreign import ccall safe "LLVMLoadLibraryPermanently" loadLibraryPermanently ::
  CString -> IO LLVMBool
