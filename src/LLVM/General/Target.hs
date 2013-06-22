-- | A Target describes that for which code may be intended. Targets are used both during actual
-- | lowering of LLVM IR to machine code and by some optimization passes which use the target to
-- | judge costs.
module LLVM.General.Target (
   lookupTarget,
   TargetOptions,
   withTargetOptions, peekTargetOptions, pokeTargetOptions,
   TargetMachine,
   withTargetMachine
 ) where

import LLVM.General.Internal.Target

