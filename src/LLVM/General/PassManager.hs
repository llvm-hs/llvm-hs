-- | A 'PassManager' holds collection of passes, to be run on 'Module's.
-- Build one with 'createPassManager':
-- 
--  * from a 'CuratedPassSetSpec' if you want optimization but not to play with your compiler
--
--  * from a ['LLVM.General.Transform.Pass'] if you do want to play with your compiler
--
--  * from a (['LLVM.General.Transform.Pass'], 'LLVM.General.Target.TargetMachine') if you
--    want to provide target-specific information (e.g. instruction costs) to the few passes
--    that use it (see comments on 'LLVM.General.Transforms.Pass').
module LLVM.General.PassManager (
  PassManager,
  PassManagerSpecification,
  CuratedPassSetSpec(..), defaultCuratedPassSetSpec,
  withPassManager,
  runPassManager
  ) where

import LLVM.General.Internal.PassManager
