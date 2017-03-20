-- | Code generation options, used in specifying TargetMachine
module LLVM.CodeGenOpt where

import LLVM.Prelude

-- | <http://llvm.org/doxygen/namespacellvm_1_1CodeGenOpt.html>
data Level
    = None
    | Less
    | Default
    | Aggressive
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
