{-# LANGUAGE
  ForeignFunctionInterface,
  MultiParamTypeClasses,
  UndecidableInstances
  #-}
-- | FFI functions for handling the LLVM GlobalVariable class
module LLVM.Internal.FFI.GlobalVariable where

import LLVM.Prelude

import Foreign.Ptr
import Foreign.C

import LLVM.Internal.FFI.PtrHierarchy
import LLVM.Internal.FFI.LLVMCTypes

foreign import ccall unsafe "LLVMIsAGlobalVariable" isAGlobalVariable ::
    Ptr Value -> IO (Ptr GlobalVariable)

foreign import ccall unsafe "LLVMIsGlobalConstant" isGlobalConstant ::
    Ptr GlobalVariable -> IO LLVMBool

foreign import ccall unsafe "LLVMSetGlobalConstant" setGlobalConstant ::
    Ptr GlobalVariable -> LLVMBool -> IO ()

foreign import ccall unsafe "LLVMGetInitializer" getInitializer ::
    Ptr GlobalVariable -> IO (Ptr Constant)

foreign import ccall unsafe "LLVMSetInitializer" setInitializer ::
    Ptr GlobalVariable -> Ptr Constant -> IO ()

