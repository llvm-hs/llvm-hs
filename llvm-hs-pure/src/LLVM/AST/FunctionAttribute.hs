-- | Module to allow importing 'FunctionAttribute' distinctly qualified.
module LLVM.AST.FunctionAttribute where

import LLVM.Prelude

-- | <http://llvm.org/docs/LangRef.html#function-attributes>
data FunctionAttribute
    = NoReturn
    | NoUnwind
    | ReadNone
    | ReadOnly
    | NoInline
    | NoRecurse
    | AlwaysInline
    | MinimizeSize
    | OptimizeForSize
    | OptimizeNone
    | StackProtect
    | StackProtectReq
    | StackProtectStrong
    | StrictFP
    | NoRedZone
    | NoImplicitFloat
    | Naked
    | InlineHint
    | StackAlignment Word64
    | ReturnsTwice
    | UWTable
    | NonLazyBind
    | Builtin
    | NoBuiltin
    | Cold
    | JumpTable
    | NoDuplicate
    | NoFree
    | SanitizeAddress
    | SanitizeHWAddress
    | SanitizeThread
    | SanitizeMemory
    | Speculatable
    | StringAttribute {
        stringAttributeKind :: ShortByteString,
        stringAttributeValue :: ShortByteString -- ^ Use "" for no value -- the two are conflated
      }
    | AllocSize Word32 (Maybe Word32) -- ^ AllocSize 0 (Just 0) is invalid
    | WriteOnly
    | ArgMemOnly
    | Convergent
    | InaccessibleMemOnly
    | InaccessibleMemOrArgMemOnly
    | SafeStack
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <http://llvm.org/docs/LangRef.html#attribute-groups>
newtype GroupID = GroupID Word
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
