-- | Module to allow importing 'ParameterAttribute' distinctly qualified.
module LLVM.AST.ParameterAttribute where

import LLVM.Prelude

-- | <http://llvm.org/docs/LangRef.html#parameter-attributes>
data ParameterAttribute
    = ZeroExt
    | SignExt
    | InReg
    | SRet
    | Alignment Word64
    | NoAlias
    | ByVal
    | NoCapture
    | Nest
    | ReadNone
    | ReadOnly
    | WriteOnly
    | InAlloca
    | NonNull
    | Dereferenceable Word64
    | DereferenceableOrNull Word64
    | Returned
    | SwiftSelf
    | SwiftError
  deriving (Eq, Ord, Read, Show, Typeable, Data)
