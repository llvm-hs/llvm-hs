-- | Module to allow importing 'ParameterAttribute' distinctly qualified.
module LLVM.AST.ParameterAttribute where

import LLVM.Prelude

-- | <http://llvm.org/docs/LangRef.html#parameter-attributes>
data ParameterAttribute
    = Alignment Word64
    | ByVal
    | Dereferenceable Word64
    | DereferenceableOrNull Word64
    | ImmArg
    | InAlloca
    | InReg
    | Nest
    | NoAlias
    | NoCapture
    | NoFree
    | NonNull
    | ReadNone
    | ReadOnly
    | Returned
    | SignExt
    | SRet
    | SwiftError
    | SwiftSelf
    | WriteOnly
    | StringAttribute {
        stringAttributeKind :: ShortByteString,
        stringAttributeValue :: ShortByteString -- ^ Use "" for no value -- the two are conflated
      }
    | ZeroExt
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
