{-# LANGUAGE ForeignFunctionInterface #-}

module LLVM.Internal.FFI.Passes where

import LLVM.Prelude

import Foreign.Ptr
import Foreign.C.String

import LLVM.Internal.FFI.Error
import LLVM.Internal.FFI.Module
import LLVM.Internal.FFI.Target


data PassBuilderOptions

foreign import ccall unsafe "LLVMCreatePassBuilderOptions" createPassBuilderOptions :: IO (Ptr PassBuilderOptions)
foreign import ccall unsafe "LLVMDisposePassBuilderOptions" disposePassBuilderOptions :: Ptr PassBuilderOptions -> IO ()

foreign import ccall unsafe "LLVMRunPasses" runPasses
  :: Ptr Module -> CString -> Ptr TargetMachine -> Ptr PassBuilderOptions -> IO (Ptr Error)

