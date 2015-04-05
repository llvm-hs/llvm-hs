-- | Module to allow importing 'Attribute' distinctly qualified.
module LLVM.General.AST.Attribute where

import LLVM.General.Prelude

-- | <http://llvm.org/docs/LangRef.html#parameter-attributes>
data ParameterAttribute
    = ZeroExt
    | SignExt
    | InReg
    | SRet
    | Alignment Word32
    | NoAlias
    | ByVal
    | NoCapture
    | Nest
  deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | <http://llvm.org/docs/LangRef.html#function-attributes>
data FunctionAttribute
    = NoReturn
    | NoUnwind
    | ReadNone
    | ReadOnly
    | NoInline
    | AlwaysInline
    | OptimizeForSize
    | StackProtect
    | StackProtectReq
    | NoRedZone
    | NoImplicitFloat
    | Naked
    | InlineHint
    | StackAlignment Word32
    | ReturnsTwice
    | UWTable
    | NonLazyBind
  deriving (Eq, Ord, Read, Show, Typeable, Data)


-- | <http://llvm.org/docs/LangRef.html#attribute-groups>
newtype GroupID = GroupID Word
  deriving (Eq, Ord, Read, Show, Typeable, Data)
