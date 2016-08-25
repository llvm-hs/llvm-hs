-- | This module and descendants define AST data types to represent LLVM code.
-- Note that these types are designed for fidelity rather than convenience - if the truth
-- of what LLVM supports is less than pretty, so be it.
module LLVM.General.AST (
  Module(..), defaultModule,
  Definition(..),
  Global(GlobalVariable, GlobalAlias, Function), 
        globalVariableDefaults,
        globalAliasDefaults,
        functionDefaults,
  UnnamedAddr(..),
  Parameter(..),
  BasicBlock(..),
  module LLVM.General.AST.Instruction,
  module LLVM.General.AST.Name,
  module LLVM.General.AST.Operand,
  module LLVM.General.AST.Type
  ) where

import LLVM.General.Prelude

import LLVM.General.AST.Name
import LLVM.General.AST.Type (Type(..), FloatingPointFormat(..))
import LLVM.General.AST.Global
import LLVM.General.AST.Operand
import LLVM.General.AST.Instruction
import LLVM.General.AST.DataLayout
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.COMDAT as COMDAT

-- | Any thing which can be at the top level of a 'Module'
data Definition 
  = GlobalDefinition Global
  | TypeDefinition Name (Maybe Type)
  | MetadataNodeDefinition MetadataNodeID [Maybe Metadata]
  | NamedMetadataDefinition String [MetadataNodeID]
  | ModuleInlineAssembly String
  | FunctionAttributes A.GroupID [A.FunctionAttribute]
  | COMDAT String COMDAT.SelectionKind
    deriving (Eq, Read, Show, Typeable, Data)

-- | <http://llvm.org/docs/LangRef.html#module-structure>
data Module = 
  Module {
    moduleName :: String,
    moduleSourceFileName :: String,
    -- | a 'DataLayout', if specified, must match that of the eventual code generator
    moduleDataLayout :: Maybe DataLayout, 
    moduleTargetTriple :: Maybe String,
    moduleDefinitions :: [Definition]
  } 
  deriving (Eq, Read, Show, Typeable, Data)

-- | helper for making 'Module's
defaultModule = 
  Module {
    moduleName = "<string>",
    moduleSourceFileName = "<string>",
    moduleDataLayout = Nothing,
    moduleTargetTriple = Nothing,
    moduleDefinitions = []
  }
