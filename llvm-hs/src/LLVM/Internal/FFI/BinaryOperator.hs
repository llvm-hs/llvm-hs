{-# LANGUAGE
  ForeignFunctionInterface,
  MultiParamTypeClasses,
  UndecidableInstances
  #-}
-- | FFI functions for handling the LLVM BinaryOperator class
module LLVM.Internal.FFI.BinaryOperator where

import LLVM.Prelude

import Foreign.Ptr
import Foreign.C

import LLVM.Internal.FFI.PtrHierarchy
import LLVM.Internal.FFI.LLVMCTypes

foreign import ccall unsafe "LLVMIsABinaryOperator" isABinaryOperator ::
    Ptr Value -> IO (Ptr BinaryOperator)

foreign import ccall unsafe "LLVM_Hs_HasNoSignedWrap" hasNoSignedWrap ::
    Ptr Value -> IO LLVMBool

foreign import ccall unsafe "LLVM_Hs_HasNoUnsignedWrap" hasNoUnsignedWrap ::
    Ptr Value -> IO LLVMBool

foreign import ccall unsafe "LLVM_Hs_IsExact" isExact ::
    Ptr Value -> IO LLVMBool

foreign import ccall unsafe "LLVM_Hs_GetFastMathFlags" getFastMathFlags ::
    Ptr Value -> IO FastMathFlags
