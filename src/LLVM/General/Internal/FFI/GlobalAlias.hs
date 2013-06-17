{-# LANGUAGE
  ForeignFunctionInterface,
  EmptyDataDecls,
  MultiParamTypeClasses,
  FlexibleInstances,
  UndecidableInstances,
  OverlappingInstances
  #-}

-- | FFI functions for handling the LLVM GlobalAlias class
module LLVM.General.Internal.FFI.GlobalAlias where

import Foreign.Ptr

import LLVM.General.Internal.FFI.PtrHierarchy

foreign import ccall unsafe "LLVMIsAGlobalAlias" isAGlobalAlias ::
    Ptr Value -> IO (Ptr GlobalAlias)

foreign import ccall unsafe "LLVM_General_GetAliasee" getAliasee ::
    Ptr GlobalAlias -> IO (Ptr Constant)

foreign import ccall unsafe "LLVM_General_SetAliasee" setAliasee ::
    Ptr GlobalAlias -> Ptr Constant -> IO ()

