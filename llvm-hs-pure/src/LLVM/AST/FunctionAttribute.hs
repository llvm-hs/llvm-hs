-- | Module to allow importing 'FunctionAttribute' distinctly qualified.
module LLVM.AST.FunctionAttribute where

import LLVM.Prelude

data MemoryAccess = None | Read | Write | ReadWrite
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, Enum)
data MemoryLocation a = Argmem a | Inaccessiblemem a | Other a
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
data MemoryEffects a
    = Exact (MemoryLocation a)
    | Intersect (MemoryEffects a) (MemoryEffects a)
    | Union (MemoryEffects a) (MemoryEffects a)
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <http://llvm.org/docs/LangRef.html#function-attributes>
data FunctionAttribute
    = -- | AllocSize 0 (Just 0) is invalid
      AllocSize Word32 (Maybe Word32)
    | AlwaysInline
    | Builtin
    | Cold
    | Convergent
    | Hot
    | InlineHint
    | JumpTable
    | MinimizeSize
    | MustProgress
    | Naked
    | NoBuiltin
    | NoCallback
    | NoCfCheck
    | NoDuplicate
    | NoFree
    | NoImplicitFloat
    | NoInline
    | NoMerge
    | NoProfile
    | NoRecurse
    | NoRedZone
    | NoReturn
    | NoSync
    | NoUnwind
    | NonLazyBind
    | NullPointerIsValid
    | OptForFuzzing
    | OptimizeForSize
    | OptimizeNone
    | ReadNone
    | ReadOnly
    | ReturnsTwice
    | SafeStack
    | SanitizeAddress
    | SanitizeHWAddress
    | SanitizeMemTag
    | SanitizeMemory
    | SanitizeThread
    | ShadowCallStack
    | Speculatable
    | SpeculativeLoadHardening
    | Memory (MemoryEffects MemoryAccess)
    | StackAlignment Word64
    | StackProtect
    | StackProtectReq
    | StackProtectStrong
    | StrictFP
    | StringAttribute
        { stringAttributeKind :: ShortByteString
        , stringAttributeValue :: ShortByteString
        -- ^ Use "" for no value -- the two are conflated
        }
    | UWTable
    | VScaleRange
        { vScaleRangeMin :: Word32
        , vScaleRangeMax :: Word32
        -- ^ Optional max is not supported. Set it to the minimum
        -- value if unspecified.
        }
    | WillReturn
    | WriteOnly
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <http://llvm.org/docs/LangRef.html#attribute-groups>
newtype GroupID = GroupID Word
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
