{-# LANGUAGE
  ForeignFunctionInterface,
  EmptyDataDecls,
  MultiParamTypeClasses,
  FlexibleInstances,
  UndecidableInstances,
  OverlappingInstances
  #-}

-- | FFI functions for handling the LLVM User class
module LLVM.General.Internal.FFI.User where

import Foreign.Ptr
import Foreign.C

import LLVM.General.Internal.FFI.PtrHierarchy

-- | a blind type to correspond to llvm::Use
data Use

foreign import ccall unsafe "LLVMIsAUser" isAUser ::
    Ptr Value -> IO (Ptr User)

foreign import ccall unsafe "LLVMGetFirstUse" getFirstUse ::
    Ptr User -> IO (Ptr Use)

foreign import ccall unsafe "LLVMGetNextUse" getNextUse ::
    Ptr Use -> IO (Ptr Use)

foreign import ccall unsafe "LLVMGetNumOperands" getNumOperands ::
    Ptr User -> IO CUInt

foreign import ccall unsafe "LLVMGetOperand" getOperand ::
    Ptr User -> CUInt -> IO (Ptr Value)

