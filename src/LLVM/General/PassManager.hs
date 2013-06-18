-- | A 'PassManager' holds collection of passes, to be run on 'Module's.
module LLVM.General.PassManager (
  PassManager,
  PassManagerSpecification,
  PassSetSpec(..),
  CuratedPassSetSpec(..), defaultCuratedPassSetSpec,
  withPassManager,
  runPassManager
  ) where

import LLVM.General.Internal.PassManager
