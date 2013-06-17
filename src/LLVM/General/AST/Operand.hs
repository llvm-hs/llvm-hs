module LLVM.General.AST.Operand where
  
import Data.Word

import LLVM.General.AST.Name
import LLVM.General.AST.Constant
import LLVM.General.AST.InlineAssembly

newtype MetadataNodeID = MetadataNodeID Word
  deriving (Eq, Ord, Read, Show)

data MetadataNode 
  = MetadataNode [Operand]
  | MetadataNodeReference MetadataNodeID
  deriving (Eq, Ord, Read, Show)

data Operand 
  = LocalReference Name
  | ConstantOperand Constant
  | MetadataStringOperand String
  | MetadataNodeOperand MetadataNode
  deriving (Eq, Ord, Read, Show)

type CallableOperand  = Either InlineAssembly Operand
