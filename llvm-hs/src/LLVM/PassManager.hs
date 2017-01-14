-- | A 'PassManager' holds collection of passes, to be run on 'Module's.
-- Build one with 'withPassManager':
-- 
--  * using 'CuratedPassSetSpec' if you want optimization but not to play with your compiler
--
--  * using 'PassSetSpec' if you do want to play with your compiler
module LLVM.PassManager (
  PassManager,
  PassSetSpec(..), defaultPassSetSpec, defaultCuratedPassSetSpec,
  withPassManager,
  runPassManager
  ) where

import LLVM.Internal.PassManager
