-- | Operations for the 'LLVM.General.AST.Instruction.AtomicRMW' instruction
module LLVM.General.AST.RMWOperation where

import Data.Data

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
  deriving (Eq, Ord, Read, Show, Data, Typeable)



