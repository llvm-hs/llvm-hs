-- | Module to allow importing 'Attribute' distinctly qualified.
module LLVM.General.AST.Attribute where

import Data.Word

-- | <http://llvm.org/docs/LangRef.html#parameter-attributes>
data ParameterAttribute
    = ZeroExt
    | SignExt
    | InReg
    | SRet
    | NoAlias
    | ByVal
    | NoCapture
    | Nest
  deriving (Eq, Read, Show)

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
    | Alignment Word32
    | NoRedZone
    | NoImplicitFloat
    | Naked
    | InlineHint
    | StackAlignment Word32
    | ReturnsTwice
    | UWTable
    | NonLazyBind
  deriving (Eq, Read, Show)
