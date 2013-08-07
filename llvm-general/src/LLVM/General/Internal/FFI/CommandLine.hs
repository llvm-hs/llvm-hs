{-# LANGUAGE
  ForeignFunctionInterface
  #-}
module LLVM.General.Internal.FFI.CommandLine where

import Foreign.Ptr
import Foreign.C

foreign import ccall unsafe "LLVM_General_ParseCommandLineOptions" parseCommandLineOptions' ::
  CUInt -> Ptr (Ptr CChar) -> Ptr CChar -> IO ()

parseCommandLineOptions = uncurry parseCommandLineOptions'
