module LLVM.General.PassManager (
  PassManager,
  PassManagerSpecification,
  CuratedPassSetSpec(..), defaultCuratedPassSetSpec,
  withPassManager,
  runPassManager
  ) where

import LLVM.General.Internal.PassManager
