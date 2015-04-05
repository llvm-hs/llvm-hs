{-# LANGUAGE
  ForeignFunctionInterface
  #-}

module LLVM.General.Internal.FFI.DataLayout where

import LLVM.General.Prelude

import Foreign.C.String
import Foreign.Ptr

import LLVM.General.Internal.FFI.LLVMCTypes

data DataLayout

-- Oooh those wacky LLVM C-API coders: C API called DataLayout TargetData.
-- Great. Just great.

foreign import ccall unsafe "LLVMCreateTargetData" createDataLayout :: 
  CString -> IO (Ptr DataLayout)

foreign import ccall unsafe "LLVMDisposeTargetData" disposeDataLayout :: 
  Ptr DataLayout -> IO ()

foreign import ccall unsafe "LLVMCopyStringRepOfTargetData" dataLayoutToString ::
  Ptr DataLayout -> IO (OwnerTransfered CString)
