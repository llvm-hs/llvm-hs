{-# LANGUAGE
  ForeignFunctionInterface,
  MultiParamTypeClasses,
  UndecidableInstances
  #-}
-- | FFI functions for handling the LLVM User class
module LLVM.General.Internal.FFI.User where

import LLVM.General.Prelude

import Foreign.Ptr
import Foreign.C

import LLVM.General.Internal.FFI.PtrHierarchy

-- | a blind type to correspond to llvm::Use
data Use

-- | test if a 'Value' is a 'User'
foreign import ccall unsafe "LLVMIsAUser" isAUser ::
    Ptr Value -> IO (Ptr User)

-- | <http://llvm.org/doxygen/group__LLVMCCoreValueUses.html#ga66a226d3d06ffada5c929656f4d97d35>
foreign import ccall unsafe "LLVMGetFirstUse" getFirstUse ::
    Ptr User -> IO (Ptr Use)

-- | <http://llvm.org/doxygen/group__LLVMCCoreValueUses.html#ga6ea72661bcca2b77bea57173317ec942>
foreign import ccall unsafe "LLVMGetNextUse" getNextUse ::
    Ptr Use -> IO (Ptr Use)

-- | <http://llvm.org/doxygen/group__LLVMCCoreValueUser.html#ga2ad633a6afc7906f1afe329f244240f6>
foreign import ccall unsafe "LLVMGetNumOperands" getNumOperands ::
    Ptr User -> IO CUInt

-- | <http://llvm.org/doxygen/group__LLVMCCoreValueUser.html#ga799d58a361054323cb457945071cbfdb>
foreign import ccall unsafe "LLVMGetOperand" getOperand ::
    Ptr User -> CUInt -> IO (Ptr Value)

