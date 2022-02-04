-- | Module to allow importing 'FunctionAttribute' distinctly qualified.
module LLVM.AST.FunctionAttribute where

import LLVM.Prelude

-- | <http://llvm.org/docs/LangRef.html#function-attributes>
data FunctionAttribute
    = AllocSize Word32 (Maybe Word32) -- ^ AllocSize 0 (Just 0) is invalid
    | AlwaysInline
    | ArgMemOnly
    | Builtin
    | Cold
    | Convergent
    | InaccessibleMemOnly
    | InaccessibleMemOrArgMemOnly
    | InlineHint
    | JumpTable
    | MinimizeSize
    | MustProgress
    | Naked
    | NoBuiltin
    | NoDuplicate
    | NoFree
    | NoImplicitFloat
    | NoInline
    | NonLazyBind
    | NoRecurse
    | NoRedZone
    | NoReturn
    | NoSync
    | NoUnwind
    | OptimizeForSize
    | OptimizeNone
    | ReadNone
    | ReadOnly
    | ReturnsTwice
    | SafeStack
    | SanitizeAddress
    | SanitizeHWAddress
    | SanitizeMemory
    | SanitizeThread
    | Speculatable
    | StackAlignment Word64
    | StackProtect
    | StackProtectReq
    | StackProtectStrong
    | StrictFP
    | StringAttribute {
        stringAttributeKind :: ShortByteString,
        stringAttributeValue :: ShortByteString -- ^ Use "" for no value -- the two are conflated
      }
    | UWTable
    | WillReturn
    | WriteOnly
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <http://llvm.org/docs/LangRef.html#attribute-groups>
newtype GroupID = GroupID Word
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
