-- | A representation of an LLVM inline assembly
module LLVM.General.AST.InlineAssembly where

import LLVM.General.Prelude

import LLVM.General.AST.Type

-- | the dialect of assembly used in an inline assembly string
-- <http://en.wikipedia.org/wiki/X86_assembly_language#Syntax>
data Dialect
  = ATTDialect
  | IntelDialect
  deriving (Eq, Read, Show, Typeable, Data)

-- | <http://llvm.org/docs/LangRef.html#inline-assembler-expressions>
-- to be used through 'LLVM.General.AST.Operand.CallableOperand' with a
-- 'LLVM.General.AST.Instruction.Call' instruction
data InlineAssembly
  = InlineAssembly {
      type' :: Type,
      assembly :: String,
      constraints :: String,
      hasSideEffects :: Bool,
      alignStack :: Bool,
      dialect :: Dialect
    }
  deriving (Eq, Read, Show, Typeable, Data)
