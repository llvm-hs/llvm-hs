{-# LANGUAGE
  ForeignFunctionInterface,
  MultiParamTypeClasses,
  UndecidableInstances
  #-}
-- | FFI functions for handling the LLVM GlobalAlias class
module LLVM.Internal.FFI.GlobalAlias where

import LLVM.Prelude

import Foreign.Ptr

import LLVM.Internal.FFI.PtrHierarchy

-- | test if a 'Value' is a 'GlobalAlias'
foreign import ccall unsafe "LLVMIsAGlobalAlias" isAGlobalAlias ::
    Ptr Value -> IO (Ptr GlobalAlias)

-- | get the constant aliased by this alias
foreign import ccall unsafe "LLVM_Hs_GetAliasee" getAliasee ::
    Ptr GlobalAlias -> IO (Ptr Constant)

-- | set the constant aliased by this alias
foreign import ccall unsafe "LLVM_Hs_SetAliasee" setAliasee ::
    Ptr GlobalAlias -> Ptr Constant -> IO ()

