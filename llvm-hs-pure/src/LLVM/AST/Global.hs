-- | 'Global's - top-level values in 'Module's - and supporting structures.
module LLVM.AST.Global where

import LLVM.Prelude

import LLVM.AST.Name
import LLVM.AST.Type
import LLVM.AST.Constant (Constant)
import LLVM.AST.AddrSpace
import LLVM.AST.Instruction (Named, Instruction, Terminator)
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Visibility as V
import qualified LLVM.AST.DLL as DLL
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.ThreadLocalStorage as TLS
import qualified LLVM.AST.Attribute as A
import LLVM.AST.Operand (MDRef, MDNode)

-- | <http://llvm.org/doxygen/classllvm_1_1GlobalValue.html>
data Global
    -- | <http://llvm.org/docs/LangRef.html#global-variables>
    = GlobalVariable {
        name :: Name,
        linkage :: L.Linkage,
        visibility :: V.Visibility,
        dllStorageClass :: Maybe DLL.StorageClass,
        threadLocalMode :: Maybe TLS.Model,
        unnamedAddr :: Maybe UnnamedAddr,
        isConstant :: Bool,
        type' :: Type,
        addrSpace :: AddrSpace,
        initializer :: Maybe Constant,
        section :: Maybe ShortByteString,
        comdat :: Maybe ShortByteString,
        alignment :: Word32,
        metadata :: [(ShortByteString, MDRef MDNode)]
      }
    -- | <http://llvm.org/docs/LangRef.html#aliases>
    | GlobalAlias {
        name :: Name,
        linkage :: L.Linkage,
        visibility :: V.Visibility,
        dllStorageClass :: Maybe DLL.StorageClass,
        threadLocalMode :: Maybe TLS.Model,
        unnamedAddr :: Maybe UnnamedAddr,
        type' :: Type,
        addrSpace :: AddrSpace,
        aliasee :: Constant
      }
    -- | <http://llvm.org/docs/LangRef.html#functions>
    | Function {
        linkage :: L.Linkage,
        visibility :: V.Visibility,
        dllStorageClass :: Maybe DLL.StorageClass,
        callingConvention :: CC.CallingConvention,
        returnAttributes :: [A.ParameterAttribute],
        returnType :: Type,
        name :: Name,
        parameters :: ([Parameter],Bool), -- ^ snd indicates varargs
        functionAttributes :: [Either A.GroupID A.FunctionAttribute],
        section :: Maybe ShortByteString,
        comdat :: Maybe ShortByteString,
        alignment :: Word32,
        garbageCollectorName :: Maybe ShortByteString,
        prefix :: Maybe Constant,
        basicBlocks :: [BasicBlock],
        personalityFunction :: Maybe Constant,
        metadata :: [(ShortByteString, MDRef MDNode)]
      }
  deriving (Eq, Read, Show, Typeable, Data, Generic)

-- | 'Parameter's for 'Function's
data Parameter = Parameter Type Name [A.ParameterAttribute]
  deriving (Eq, Read, Show, Typeable, Data, Generic)

-- | <http://llvm.org/doxygen/classllvm_1_1BasicBlock.html>
-- LLVM code in a function is a sequence of 'BasicBlock's each with a label,
-- some instructions, and a terminator.
data BasicBlock = BasicBlock Name [Named Instruction] (Named Terminator)
  deriving (Eq, Read, Show, Typeable, Data, Generic)

data UnnamedAddr = LocalAddr | GlobalAddr
  deriving (Eq, Read, Show, Typeable, Data, Generic)

-- | helper for making 'GlobalVariable's
globalVariableDefaults :: Global
globalVariableDefaults = 
  GlobalVariable {
  name = error "global variable name not defined",
  linkage = L.External,
  visibility = V.Default,
  dllStorageClass = Nothing,
  threadLocalMode = Nothing,
  addrSpace = AddrSpace 0,
  unnamedAddr = Nothing,
  isConstant = False,
  type' = error "global variable type not defined",
  initializer = Nothing,
  section = Nothing,
  comdat = Nothing,
  alignment = 0,
  metadata = []
  }

-- | helper for making 'GlobalAlias's
globalAliasDefaults :: Global
globalAliasDefaults =
  GlobalAlias {
    name = error "global alias name not defined",
    linkage = L.External,
    visibility = V.Default,
    dllStorageClass = Nothing,
    threadLocalMode = Nothing,
    unnamedAddr = Nothing,
    type' = error "global alias type not defined",
    addrSpace = AddrSpace 0,
    aliasee = error "global alias aliasee not defined"
  }

-- | helper for making 'Function's
functionDefaults :: Global
functionDefaults = 
  Function {
    linkage = L.External,
    visibility = V.Default,
    dllStorageClass = Nothing,
    callingConvention = CC.C,
    returnAttributes = [],
    returnType = error "function return type not defined",
    name = error "function name not defined",
    parameters = ([], False),
    functionAttributes = [],
    section = Nothing,
    comdat = Nothing,
    alignment = 0,
    garbageCollectorName = Nothing,
    prefix = Nothing,
    basicBlocks = [],
    personalityFunction = Nothing,
    metadata = []
  }
