{-# LANGUAGE
  ForeignFunctionInterface,
  MultiParamTypeClasses,
  UndecidableInstances
  #-}
-- | FFI functions for handling the LLVM GlobalAlias class
module LLVM.General.Internal.FFI.GlobalAlias where

import LLVM.General.Prelude

import Foreign.Ptr

import LLVM.General.Internal.FFI.PtrHierarchy

-- | test if a 'Value' is a 'GlobalAlias'
foreign import ccall unsafe "LLVMIsAGlobalAlias" isAGlobalAlias ::
    Ptr Value -> IO (Ptr GlobalAlias)

-- | get the constant aliased by this alias
foreign import ccall unsafe "LLVM_General_GetAliasee" getAliasee ::
    Ptr GlobalAlias -> IO (Ptr Constant)

-- | set the constant aliased by this alias
foreign import ccall unsafe "LLVM_General_SetAliasee" setAliasee ::
    Ptr GlobalAlias -> Ptr Constant -> IO ()

