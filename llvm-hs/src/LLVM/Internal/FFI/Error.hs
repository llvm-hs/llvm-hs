module LLVM.Internal.FFI.Error where

import LLVM.Prelude

import Foreign.Ptr
import Foreign.C.String

data Error

foreign import ccall unsafe "LLVMConsumeError" consumeError :: Ptr Error -> IO ()

foreign import ccall unsafe "LLVMGetErrorMessage" getErrorMessage :: Ptr Error -> IO CString

foreign import ccall unsafe "LLVMDisposeErrorMessage" disposeErrorMessage :: CString -> IO ()
