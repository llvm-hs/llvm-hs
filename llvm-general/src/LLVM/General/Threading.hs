-- | functionality necessary when running LLVM in multiple threads at the same time.
module LLVM.General.Threading (
  setMultithreaded,
  isMultithreaded
  ) where

import LLVM.General.Internal.Threading
