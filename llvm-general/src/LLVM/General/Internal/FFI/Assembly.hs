{-# LANGUAGE
  ForeignFunctionInterface
  #-}

-- | Functions to read and write textual LLVM assembly
module LLVM.General.Internal.FFI.Assembly where

import LLVM.General.Prelude

import LLVM.General.Internal.FFI.Context 
import LLVM.General.Internal.FFI.MemoryBuffer
import LLVM.General.Internal.FFI.Module
import LLVM.General.Internal.FFI.RawOStream
import LLVM.General.Internal.FFI.SMDiagnostic
import LLVM.General.Internal.FFI.LLVMCTypes

import Foreign.C
import Foreign.Ptr

-- | Use LLVM's parser to parse a string of llvm assembly in a memory buffer to get a module
foreign import ccall unsafe "LLVM_General_ParseLLVMAssembly" parseLLVMAssembly ::
  Ptr Context -> OwnerTransfered (Ptr MemoryBuffer) -> Ptr (OwnerTransfered CString) -> IO (Ptr Module)


-- | LLVM's serializer to generate a string of llvm assembly from a module
foreign import ccall unsafe "LLVM_General_WriteLLVMAssembly" writeLLVMAssembly ::
  Ptr Module -> Ptr RawOStream -> IO ()



