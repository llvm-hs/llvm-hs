-- | 'Global's - top-level values in 'Module's - and supporting structures.
module LLVM.General.AST.Global where

import Data.Data
import Data.Word

import LLVM.General.AST.Name
import LLVM.General.AST.Type
import LLVM.General.AST.Constant (Constant)
import LLVM.General.AST.AddrSpace
import LLVM.General.AST.Instruction (Named, Instruction, Terminator)
import qualified LLVM.General.AST.Linkage as L
import qualified LLVM.General.AST.Visibility as V
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Attribute as A

-- | <http://llvm.org/doxygen/classllvm_1_1GlobalValue.html>
data Global
    -- | <http://llvm.org/docs/LangRef.html#global-variables>
    = GlobalVariable {
        name :: Name,
        linkage :: L.Linkage,
        visibility :: V.Visibility,
        isThreadLocal :: Bool,
        addrSpace :: AddrSpace,
        hasUnnamedAddr :: Bool,
        isConstant :: Bool,
        type' :: Type,
        initializer :: Maybe Constant,
        section :: Maybe String,
        alignment :: Word32
      }
    -- | <http://llvm.org/docs/LangRef.html#aliases>
    | GlobalAlias {
        name :: Name,
        linkage :: L.Linkage,
        visibility :: V.Visibility,
        type' :: Type,
        aliasee :: Constant
      }
    -- | <http://llvm.org/docs/LangRef.html#functions>
    | Function {
        linkage :: L.Linkage,
        visibility :: V.Visibility,
        callingConvention :: CC.CallingConvention,
        returnAttributes :: [A.ParameterAttribute],
        returnType :: Type,
        name :: Name,
        parameters :: ([Parameter],Bool), -- ^ snd indicates varargs
        functionAttributes :: [Either A.GroupID A.FunctionAttribute],
        section :: Maybe String,
        alignment :: Word32,
        garbageCollectorName :: Maybe String,
        basicBlocks :: [BasicBlock]
      }
  deriving (Eq, Read, Show, Typeable, Data)

-- | 'Parameter's for 'Function's
data Parameter = Parameter Type Name [A.ParameterAttribute]
  deriving (Eq, Read, Show, Typeable, Data)

-- | <http://llvm.org/doxygen/classllvm_1_1BasicBlock.html>
-- LLVM code in a function is a sequence of 'BasicBlock's each with a label,
-- some instructions, and a terminator.
data BasicBlock = BasicBlock Name [Named Instruction] (Named Terminator)
  deriving (Eq, Read, Show, Typeable, Data)

-- | helper for making 'GlobalVariable's
globalVariableDefaults :: Global
globalVariableDefaults = 
  GlobalVariable {
  name = error "global variable name not defined",
  linkage = L.External,
  visibility = V.Default,
  isThreadLocal = False,
  addrSpace = AddrSpace 0,
  hasUnnamedAddr = False,
  isConstant = False,
  type' = error "global variable type not defined",
  initializer = Nothing,
  section = Nothing,
  alignment = 0
  }

-- | helper for making 'GlobalAlias's
globalAliasDefaults :: Global
globalAliasDefaults =
  GlobalAlias {
    name = error "global alias name not defined",
    linkage = L.External,
    visibility = V.Default,
    type' = error "global alias type not defined",
    aliasee = error "global alias aliasee not defined"
  }

-- | helper for making 'Function's
functionDefaults :: Global
functionDefaults = 
  Function {
    linkage = L.External,
    visibility = V.Default,
    callingConvention = CC.C,
    returnAttributes = [],
    returnType = error "function return type not defined",
    name = error "function name not defined",
    parameters = ([], False),
    functionAttributes = [],
    section = Nothing,
    alignment = 0,
    garbageCollectorName = Nothing,
    basicBlocks = []
  }
