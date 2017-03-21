-- | Relocations, used in specifying TargetMachine
module LLVM.Relocation where

import LLVM.Prelude

-- | <http://llvm.org/doxygen/namespacellvm_1_1Reloc.html>
data Model 
    = Default
    | Static
    | PIC
    | DynamicNoPIC
    deriving (Eq, Read, Show, Typeable, Data, Generic)
