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
    | NoFree
    | Nest
    | ReadNone
    | ReadOnly
    | WriteOnly
    | ImmArg
    | InAlloca
    | NonNull
    | Dereferenceable Word64
    | DereferenceableOrNull Word64
    | Returned
    | SwiftSelf
    | SwiftError
    | StringAttribute {
        stringAttributeKind :: ShortByteString,
        stringAttributeValue :: ShortByteString -- ^ Use "" for no value -- the two are conflated
      }
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
