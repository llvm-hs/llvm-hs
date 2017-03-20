-- | Predicates for the 'LLVM.AST.Instruction.ICmp' instruction
module LLVM.AST.IntegerPredicate where

import LLVM.Prelude

-- | <http://llvm.org/docs/LangRef.html#icmp-instruction>
data IntegerPredicate
  = EQ
  | NE
  | UGT
  | UGE
  | ULT
  | ULE
  | SGT
  | SGE
  | SLT
  | SLE
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)



