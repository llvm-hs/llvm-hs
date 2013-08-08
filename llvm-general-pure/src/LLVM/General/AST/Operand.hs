-- | A type to represent operands to LLVM 'LLVM.General.AST.Instruction.Instruction's
module LLVM.General.AST.Operand where
  
import Data.Word

import LLVM.General.AST.Name
import LLVM.General.AST.Constant
import LLVM.General.AST.InlineAssembly

-- | A 'MetadataNodeID' is a number for identifying a metadata node.
-- Note this is different from "named metadata", which are represented with
-- 'LLVM.General.AST.NamedMetadataDefinition'.
newtype MetadataNodeID = MetadataNodeID Word
  deriving (Eq, Ord, Read, Show)

-- | <http://llvm.org/docs/LangRef.html#metadata>
data MetadataNode 
  = MetadataNode [Maybe Operand]
  | MetadataNodeReference MetadataNodeID
  deriving (Eq, Ord, Read, Show)

-- | An 'Operand' is roughly that which is an argument to an 'LLVM.General.AST.Instruction.Instruction'
data Operand 
  -- | %foo
  = LocalReference Name
  -- | 'Constant's include 'LLVM.General.AST.Constant.GlobalReference', for \@foo
  | ConstantOperand Constant
  | MetadataStringOperand String
  | MetadataNodeOperand MetadataNode
  deriving (Eq, Ord, Read, Show)

-- | The 'LLVM.General.AST.Instruction.Call' instruction is special: the callee can be inline assembly
type CallableOperand  = Either InlineAssembly Operand
