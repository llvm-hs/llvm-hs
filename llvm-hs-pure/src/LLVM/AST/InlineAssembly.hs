-- | A representation of an LLVM inline assembly
module LLVM.AST.InlineAssembly where

import LLVM.Prelude

import LLVM.AST.Type

-- | the dialect of assembly used in an inline assembly string
-- <http://en.wikipedia.org/wiki/X86_assembly_language#Syntax>
data Dialect
  = ATTDialect
  | IntelDialect
  deriving (Eq, Read, Show, Typeable, Data, Generic)

-- | <http://llvm.org/docs/LangRef.html#inline-assembler-expressions>
-- to be used through 'LLVM.AST.Operand.CallableOperand' with a
-- 'LLVM.AST.Instruction.Call' instruction
data InlineAssembly
  = InlineAssembly {
      type' :: Type,
      assembly :: ByteString,
      constraints :: ShortByteString,
      hasSideEffects :: Bool,
      alignStack :: Bool,
      dialect :: Dialect
    }
  deriving (Eq, Read, Show, Typeable, Data, Generic)
