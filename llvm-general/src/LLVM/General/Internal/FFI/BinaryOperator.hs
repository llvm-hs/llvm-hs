{-# LANGUAGE
  ForeignFunctionInterface,
  MultiParamTypeClasses,
  UndecidableInstances
  #-}
-- | FFI functions for handling the LLVM BinaryOperator class
module LLVM.General.Internal.FFI.BinaryOperator where

import LLVM.General.Prelude

import Foreign.Ptr
import Foreign.C

import LLVM.General.Internal.FFI.PtrHierarchy
import LLVM.General.Internal.FFI.LLVMCTypes

foreign import ccall unsafe "LLVMIsABinaryOperator" isABinaryOperator ::
    Ptr Value -> IO (Ptr BinaryOperator)

foreign import ccall unsafe "LLVM_General_HasNoSignedWrap" hasNoSignedWrap ::
    Ptr Value -> IO LLVMBool

foreign import ccall unsafe "LLVM_General_HasNoUnsignedWrap" hasNoUnsignedWrap ::
    Ptr Value -> IO LLVMBool

foreign import ccall unsafe "LLVM_General_IsExact" isExact ::
    Ptr Value -> IO LLVMBool

foreign import ccall unsafe "LLVM_General_GetFastMathFlags" getFastMathFlags ::
    Ptr Value -> IO FastMathFlags
