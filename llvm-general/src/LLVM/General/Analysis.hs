-- | functionality for analyzing 'LLVM.General.Module.Module's.  Much of the analysis
-- possible with LLVM is managed internally, as needed by 'Transforms', and so is not
-- yet exposed here.
module LLVM.General.Analysis (
  verify
  ) where

import LLVM.General.Internal.Analysis
