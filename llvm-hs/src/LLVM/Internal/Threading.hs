module LLVM.Internal.Threading (
  isMultithreaded
  ) where

import LLVM.Prelude

import qualified LLVM.Internal.FFI.Threading as FFI

import LLVM.Internal.Coding

-- | Check if multithreading is enabled in LLVM
isMultithreaded :: IO Bool
isMultithreaded = FFI.isMultithreaded >>= decodeM
