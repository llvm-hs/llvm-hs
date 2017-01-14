{-# LANGUAGE
  ForeignFunctionInterface
  #-}
module LLVM.Internal.FFI.ByteRangeCallback where

import LLVM.Prelude

import Foreign.C
import Foreign.Ptr

type ByteRangeCallback = Ptr CChar -> CSize -> IO ()
foreign import ccall "wrapper" wrapByteRangeCallback :: 
  ByteRangeCallback -> IO (FunPtr ByteRangeCallback)

