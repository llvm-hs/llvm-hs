-- | Utility functions for resolving external symbols
module LLVM.Linking
  ( loadLibraryPermanently
  , getSymbolAddressInProcess
  ) where

import LLVM.Internal.Linking
