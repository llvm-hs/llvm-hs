module LLVM.General.Internal.Threading (
  isMultithreaded
  ) where

import LLVM.General.Prelude

import qualified LLVM.General.Internal.FFI.Threading as FFI

import LLVM.General.Internal.Coding

-- | Check if multithreading is enabled in LLVM
isMultithreaded :: IO Bool
isMultithreaded = FFI.isMultithreaded >>= decodeM
