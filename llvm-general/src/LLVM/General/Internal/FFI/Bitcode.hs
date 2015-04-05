{-# LANGUAGE
  ForeignFunctionInterface
  #-}

-- | Functions to read and write LLVM bitcode
module LLVM.General.Internal.FFI.Bitcode where

import LLVM.General.Prelude

import LLVM.General.Internal.FFI.RawOStream
import LLVM.General.Internal.FFI.Context 
import LLVM.General.Internal.FFI.MemoryBuffer
import LLVM.General.Internal.FFI.Module
import LLVM.General.Internal.FFI.LLVMCTypes

import Foreign.C
import Foreign.Ptr

foreign import ccall unsafe "LLVM_General_ParseBitcode" parseBitcode ::
  Ptr Context -> Ptr MemoryBuffer -> Ptr (OwnerTransfered CString) -> IO (Ptr Module)

foreign import ccall unsafe "LLVM_General_WriteBitcode" writeBitcode ::
  Ptr Module -> Ptr RawOStream -> IO ()

                               

