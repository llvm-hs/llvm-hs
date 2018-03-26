{-# LANGUAGE OverloadedStrings #-}
-- | This module and descendants define AST data types to represent LLVM code.
-- Note that these types are designed for fidelity rather than convenience - if the truth
-- of what LLVM supports is less than pretty, so be it.
module LLVM.AST (
  Module(..), defaultModule,
  Definition(..),
  Global(GlobalVariable, GlobalAlias, Function),
        globalVariableDefaults,
        globalAliasDefaults,
        functionDefaults,
  UnnamedAddr(..),
  Parameter(..),
  BasicBlock(..),
  Operand(..),
  CallableOperand,
  Metadata(..),
  MetadataNodeID(..),
  MDRef(..),
  MDNode(..),
  module LLVM.AST.Instruction,
  module LLVM.AST.Name,
  module LLVM.AST.Type
  ) where

import LLVM.Prelude

import LLVM.AST.Name
import LLVM.AST.Type (Type(..), FloatingPointType(..))
import LLVM.AST.Global
import LLVM.AST.Operand hiding (Module)
import LLVM.AST.Instruction
import LLVM.AST.DataLayout
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.COMDAT as COMDAT

-- | Any thing which can be at the top level of a 'Module'
data Definition
  = GlobalDefinition Global
  | TypeDefinition Name (Maybe Type)
  | MetadataNodeDefinition MetadataNodeID MDNode
  | NamedMetadataDefinition ShortByteString [MetadataNodeID]
  | ModuleInlineAssembly ByteString
  | FunctionAttributes A.GroupID [A.FunctionAttribute]
  | COMDAT ShortByteString COMDAT.SelectionKind
    deriving (Eq, Read, Show, Typeable, Data, Generic)

-- | <http://llvm.org/docs/LangRef.html#module-structure>
data Module =
  Module {
    moduleName :: ShortByteString,
    moduleSourceFileName :: ShortByteString,
    -- | a 'DataLayout', if specified, must match that of the eventual code generator
    moduleDataLayout :: Maybe DataLayout,
    moduleTargetTriple :: Maybe ShortByteString,
    moduleDefinitions :: [Definition]
  }
  deriving (Eq, Read, Show, Typeable, Data, Generic)

-- | helper for making 'Module's
defaultModule :: Module
defaultModule =
  Module {
    moduleName = "<string>",
    moduleSourceFileName = "<string>",
    moduleDataLayout = Nothing,
    moduleTargetTriple = Nothing,
    moduleDefinitions = []
  }
