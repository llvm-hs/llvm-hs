{-# LANGUAGE
  ForeignFunctionInterface
  #-}
module LLVM.General.Internal.FFI.MemoryBuffer where

import Foreign.Ptr
import Foreign.C

data MemoryBuffer

foreign import ccall unsafe "LLVMGetBufferStart" getBufferStart ::
  Ptr MemoryBuffer -> IO (Ptr CChar)

foreign import ccall unsafe "LLVMGetBufferSize" getBufferSize ::
  Ptr MemoryBuffer -> IO CSize

foreign import ccall unsafe "LLVMDisposeMemoryBuffer" disposeMemoryBuffer ::
  Ptr MemoryBuffer -> IO ()
