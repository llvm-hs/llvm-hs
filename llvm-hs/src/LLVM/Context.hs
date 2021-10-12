-- | functions for the LLVM Context object which holds thread-scope state
module LLVM.Context (
  Context,
  withContext,
  createContext,
  disposeContext
  ) where

import LLVM.Internal.Context
