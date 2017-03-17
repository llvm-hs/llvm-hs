{-# LANGUAGE
  ForeignFunctionInterface
  #-}

-- | Functions to read and write textual LLVM assembly
module LLVM.Internal.FFI.Assembly where

import LLVM.Prelude

import LLVM.Internal.FFI.Context
import LLVM.Internal.FFI.LLVMCTypes
import LLVM.Internal.FFI.MemoryBuffer
import LLVM.Internal.FFI.Module
import LLVM.Internal.FFI.PtrHierarchy

import Foreign.C
import Foreign.Ptr

-- | Use LLVM's parser to parse a string of llvm assembly in a memory buffer to get a module
foreign import ccall unsafe "LLVM_Hs_ParseLLVMAssembly" parseLLVMAssembly ::
  Ptr Context -> OwnerTransfered (Ptr MemoryBuffer) -> Ptr (OwnerTransfered CString) -> IO (Ptr Module)


-- | LLVM's serializer to generate a string of llvm assembly from a module
foreign import ccall unsafe "LLVM_Hs_WriteLLVMAssembly" writeLLVMAssembly ::
  Ptr Module -> Ptr RawOStream -> IO ()



