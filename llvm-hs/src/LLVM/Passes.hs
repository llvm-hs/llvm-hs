-- | This module provides an interface to LLVM's passes.
module LLVM.Passes (PassSetSpec (..), ModulePass (..), runPasses) where

import LLVM.Internal.Passes
