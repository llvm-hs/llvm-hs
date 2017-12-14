-- | A type to represent operands to LLVM 'LLVM.AST.Instruction.Instruction's
module LLVM.AST.Operand where

import LLVM.Prelude

import LLVM.AST.Name
import LLVM.AST.Constant
import LLVM.AST.InlineAssembly
import LLVM.AST.Type

-- | A 'MetadataNodeID' is a number for identifying a metadata node.
-- Note this is different from "named metadata", which are represented with
-- 'LLVM.AST.NamedMetadataDefinition'.
newtype MetadataNodeID = MetadataNodeID Word
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <http://llvm.org/docs/LangRef.html#metadata>
data MetadataNode 
  = MetadataNode [Maybe Metadata]
  | MetadataNodeReference MetadataNodeID
  | MetadataNodeSpecialized SMetaNode
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <http://llvm.org/docs/LangRef.html#metadata>
data Metadata
  = MDString ShortByteString -- ^ <http://llvm.org/docs/doxygen/html/classllvm_1_1MDNode.html>
  | MDNode MetadataNode -- ^ <http://llvm.org/docs/doxygen/html/classllvm_1_1MDNode.html>
  | MDValue Operand -- ^ <http://llvm.org/docs/doxygen/html/classllvm_1_1ValueAsMetadata.html>
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | An 'Operand' is roughly that which is an argument to an 'LLVM.AST.Instruction.Instruction'
data Operand 
  -- | %foo
  = LocalReference Type Name
  -- | 'Constant's include 'LLVM.AST.Constant.GlobalReference', for \@foo
  | ConstantOperand Constant
  | MetadataOperand Metadata
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | The 'LLVM.AST.Instruction.Call' instruction is special: the callee can be inline assembly
type CallableOperand  = Either InlineAssembly Operand

-- | <http://llvm.org/docs/LangRef.html#specialized-metadata-nodes
data SMetaNode
  = DIFile {
      filename :: ShortByteString,
      directory :: ShortByteString,
      checksumKind :: DIFileChecksum,
      checksum :: Maybe ShortByteString
    }
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DIFileChecksum = CSK_None | CSK_MD5 | CSK_SHA1
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)