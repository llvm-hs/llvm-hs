-- | Predicates for the 'LLVM.General.AST.Instruction.ICmp' instruction
module LLVM.General.AST.IntegerPredicate where

import Data.Data

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
  deriving (Eq, Ord, Read, Show, Data, Typeable)



