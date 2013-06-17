module LLVM.General.AST.Global where

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

data Global
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
    | GlobalAlias {
        name :: Name,
        linkage :: L.Linkage,
        visibility :: V.Visibility,
        type' :: Type,
        aliasee :: Constant
      }
    | Function {
        linkage :: L.Linkage,
        visibility :: V.Visibility,
        callingConvention :: CC.CallingConvention,
        returnAttributes :: [A.ParameterAttribute],
        returnType :: Type,
        name :: Name,
        parameters :: ([Parameter],Bool),
        functionAttributes :: [A.FunctionAttribute],
        section :: Maybe String,
        alignment :: Word32,
        basicBlocks :: [BasicBlock]
      }
  deriving (Eq, Read, Show)

data Parameter = Parameter Type Name [A.ParameterAttribute]
  deriving (Eq, Read, Show)

data BasicBlock = BasicBlock Name [Named Instruction] (Named Terminator)
  deriving (Eq, Read, Show)

globalVariableDefaults :: Global
globalVariableDefaults = 
  GlobalVariable {
  name = undefined,
  linkage = L.External,
  visibility = V.Default,
  isThreadLocal = False,
  addrSpace = AddrSpace 0,
  hasUnnamedAddr = False,
  isConstant = False,
  type' = undefined,
  initializer = Nothing,
  section = Nothing,
  alignment = 0
  }

globalAliasDefaults :: Global
globalAliasDefaults =
  GlobalAlias {
    name = undefined,
    linkage = L.External,
    visibility = V.Default,
    type' = undefined,
    aliasee = undefined
  }

functionDefaults :: Global
functionDefaults = 
  Function {
    linkage = L.External,
    visibility = V.Default,
    callingConvention = CC.C,
    returnAttributes = [],
    returnType = undefined,
    name = undefined,
    parameters = undefined,
    functionAttributes = [],
    section = Nothing,
    alignment = 0,
    basicBlocks = []
  }
