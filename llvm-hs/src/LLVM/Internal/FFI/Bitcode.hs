{-# LANGUAGE
  ForeignFunctionInterface
  #-}

-- | Functions to read and write LLVM bitcode
module LLVM.Internal.FFI.Bitcode where

import LLVM.Prelude

import LLVM.Internal.FFI.Context
import LLVM.Internal.FFI.LLVMCTypes
import LLVM.Internal.FFI.MemoryBuffer
import LLVM.Internal.FFI.Module
import LLVM.Internal.FFI.PtrHierarchy

import Foreign.C
import Foreign.Ptr

foreign import ccall unsafe "LLVM_Hs_ParseBitcode" parseBitcode ::
  Ptr Context -> Ptr MemoryBuffer -> Ptr (OwnerTransfered CString) -> IO (Ptr Module)

foreign import ccall unsafe "LLVM_Hs_WriteBitcode" writeBitcode ::
  Ptr Module -> Ptr RawOStream -> IO ()

                               

