-- | Operations for the 'LLVM.AST.Instruction.AtomicRMW' instruction
module LLVM.AST.RMWOperation where

import LLVM.Prelude

-- | <http://llvm.org/docs/LangRef.html#atomicrmw-instruction>
data RMWOperation
  = Xchg
  | Add
  | Sub
  | And
  | Nand
  | Or
  | Xor
  | Max
  | Min
  | UMax
  | UMin
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)



