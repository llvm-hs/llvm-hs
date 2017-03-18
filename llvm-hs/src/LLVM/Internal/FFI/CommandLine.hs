{-# LANGUAGE
  ForeignFunctionInterface
  #-}
module LLVM.Internal.FFI.CommandLine where

import LLVM.Prelude

import Foreign.Ptr
import Foreign.C

foreign import ccall unsafe "LLVM_Hs_ParseCommandLineOptions" parseCommandLineOptions' ::
  CUInt -> Ptr (Ptr CChar) -> Ptr CChar -> IO ()

parseCommandLineOptions :: (CUInt, Ptr (Ptr CChar)) -> Ptr CChar -> IO ()
parseCommandLineOptions = uncurry parseCommandLineOptions'
