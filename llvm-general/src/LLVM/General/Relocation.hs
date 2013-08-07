-- | Relocations, used in specifying TargetMachine
module LLVM.General.Relocation where

import Data.Data

-- | <http://llvm.org/doxygen/namespacellvm_1_1Reloc.html>
data Model 
    = Default
    | Static
    | PIC
    | DynamicNoPIC
    deriving (Eq, Read, Show, Typeable, Data)
