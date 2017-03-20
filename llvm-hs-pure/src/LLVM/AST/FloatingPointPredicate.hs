-- | Predicates for the 'LLVM.AST.Instruction.FCmp' instruction
module LLVM.AST.FloatingPointPredicate where

import LLVM.Prelude

-- | <http://llvm.org/docs/LangRef.html#fcmp-instruction>
data FloatingPointPredicate
  = False
  | OEQ
  | OGT
  | OGE
  | OLT
  | OLE
  | ONE
  | ORD
  | UNO
  | UEQ
  | UGT
  | UGE
  | ULT
  | ULE
  | UNE
  | True
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)



