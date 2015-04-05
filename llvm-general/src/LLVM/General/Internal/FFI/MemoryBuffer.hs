{-# LANGUAGE
  ForeignFunctionInterface
  #-}
module LLVM.General.Internal.FFI.MemoryBuffer where

import LLVM.General.Prelude

import Foreign.Ptr
import Foreign.C

import LLVM.General.Internal.FFI.LLVMCTypes

data MemoryBuffer

foreign import ccall unsafe "LLVMCreateMemoryBufferWithContentsOfFile" createMemoryBufferWithContentsOfFile ::
  Ptr CChar -> Ptr (Ptr MemoryBuffer) -> Ptr (OwnerTransfered CString) -> IO LLVMBool

foreign import ccall unsafe "LLVMCreateMemoryBufferWithMemoryRange" createMemoryBufferWithMemoryRange ::
  Ptr CChar -> CSize -> CString -> LLVMBool -> IO (Ptr MemoryBuffer)

foreign import ccall unsafe "LLVMGetBufferStart" getBufferStart ::
  Ptr MemoryBuffer -> IO (Ptr CChar)

foreign import ccall unsafe "LLVMGetBufferSize" getBufferSize ::
  Ptr MemoryBuffer -> IO CSize

foreign import ccall unsafe "LLVMDisposeMemoryBuffer" disposeMemoryBuffer ::
  Ptr MemoryBuffer -> IO ()
