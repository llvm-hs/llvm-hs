{-# LANGUAGE
  ForeignFunctionInterface
  #-}
module LLVM.General.Internal.FFI.Analysis where

import LLVM.General.Prelude

import Foreign.Ptr
import Foreign.C

import LLVM.General.Internal.FFI.LLVMCTypes
import LLVM.General.Internal.FFI.Module

foreign import ccall unsafe "LLVMVerifyModule" verifyModule ::
  Ptr Module -> VerifierFailureAction -> Ptr (OwnerTransfered CString) -> IO LLVMBool
