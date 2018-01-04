{-# LANGUAGE ForeignFunctionInterface #-}

-- | FFI functions for handling the RTDyldMemoryManager class
module LLVM.Internal.FFI.RTDyldMemoryManager where

import LLVM.Prelude

import Foreign.C.String

-- | <https://llvm.org/doxygen/classllvm_1_1RTDyldMemoryManager.html#a5fee247bdc0c5af393b66bfd73a0a347>
foreign import ccall safe "LLVM_Hs_GetSymbolAddressInProcess" getSymbolAddressInProcess ::
  CString -> IO Word64
