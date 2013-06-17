{-# LANGUAGE
  ForeignFunctionInterface,
  EmptyDataDecls,
  MultiParamTypeClasses,
  FlexibleInstances,
  UndecidableInstances,
  OverlappingInstances
  #-}

-- | FFI functions for handling the LLVM BinaryOperator class
module LLVM.General.Internal.FFI.BinaryOperator where

import Foreign.Ptr
import Foreign.C

import LLVM.General.Internal.FFI.PtrHierarchy
import LLVM.General.Internal.FFI.LLVMCTypes

-- | a blind type to correspond to llvm::BinaryOperator
data BinaryOperator

instance ChildOf Instruction BinaryOperator

foreign import ccall unsafe "LLVMIsABinaryOperator" isABinaryOperator ::
    Ptr Value -> IO (Ptr BinaryOperator)

foreign import ccall unsafe "LLVM_General_HasNoSignedWrap" hasNoSignedWrap ::
    Ptr BinaryOperator -> IO LLVMBool

foreign import ccall unsafe "LLVM_General_HasNoUnsignedWrap" hasNoUnsignedWrap ::
    Ptr BinaryOperator -> IO LLVMBool

foreign import ccall unsafe "LLVM_General_IsExact" isExact ::
    Ptr BinaryOperator -> IO LLVMBool

