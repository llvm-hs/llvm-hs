-- | functions for the LLVM Context object which holds thread-scope state
module LLVM.General.Context (
  Context,
  withContext
  ) where

import LLVM.General.Internal.Context