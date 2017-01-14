{-# LANGUAGE
  ForeignFunctionInterface
  #-}
module LLVM.Internal.FFI.Analysis where

import LLVM.Prelude

import Foreign.Ptr
import Foreign.C

import LLVM.Internal.FFI.LLVMCTypes
import LLVM.Internal.FFI.Module

foreign import ccall unsafe "LLVMVerifyModule" verifyModule ::
  Ptr Module -> VerifierFailureAction -> Ptr (OwnerTransfered CString) -> IO LLVMBool
