-- | functionality for analyzing 'LLVM.Module.Module's.  Much of the analysis
-- possible with LLVM is managed internally, as needed by 'Transforms', and so is not
-- yet exposed here.
module LLVM.Analysis (
  verify
  ) where

import LLVM.Internal.Analysis
