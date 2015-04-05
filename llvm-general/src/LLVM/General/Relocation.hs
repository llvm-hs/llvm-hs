-- | Relocations, used in specifying TargetMachine
module LLVM.General.Relocation where

import LLVM.General.Prelude

-- | <http://llvm.org/doxygen/namespacellvm_1_1Reloc.html>
data Model 
    = Default
    | Static
    | PIC
    | DynamicNoPIC
    deriving (Eq, Read, Show, Typeable, Data)
