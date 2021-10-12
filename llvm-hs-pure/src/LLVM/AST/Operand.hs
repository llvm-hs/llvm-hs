{-# LANGUAGE DuplicateRecordFields #-}
-- | A type to represent operands to LLVM 'LLVM.AST.Instruction.Instruction's
module LLVM.AST.Operand
( module LLVM.AST.Operand
)
where

import LLVM.Prelude

import LLVM.AST.Name
import LLVM.AST.Constant
import LLVM.AST.InlineAssembly
import LLVM.AST.Type


-- | An 'Operand' is roughly that which is an argument to an 'LLVM.AST.Instruction.Instruction'
data Operand
  -- | %foo
  = LocalReference Type Name
  -- | 'Constant's include 'LLVM.AST.Constant.GlobalReference', for \@foo
  | ConstantOperand Constant
  | MetadataOperand Metadata
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | The 'LLVM.AST.Instruction.Call' instruction is special: the callee can be inline assembly
type CallableOperand  = Either InlineAssembly Operand

-- | <http://llvm.org/docs/LangRef.html#metadata>
data Metadata
  = MDString ShortByteString -- ^ <http://llvm.org/docs/doxygen/html/classllvm_1_1MDNode.html>
  | MDNode (MDRef MDNode) -- ^ <http://llvm.org/docs/doxygen/html/classllvm_1_1MDNode.html>
  | MDValue Operand -- ^ <http://llvm.org/docs/doxygen/html/classllvm_1_1ValueAsMetadata.html>
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | A 'MetadataNodeID' is a number for identifying a metadata node.
-- Note this is different from "named metadata", which are represented with
-- 'LLVM.AST.NamedMetadataDefinition'.
newtype MetadataNodeID = MetadataNodeID Word
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | `MDRef` can either represent a reference to some piece of
-- metadata or the metadata itself.
--
-- This is mainly useful for encoding cyclic metadata. Note that LLVM
-- represents inline and non-inline nodes identically, so
-- roundtripping the Haskell AST does not preserve whether a node was
-- inline or not.
data MDRef a
  = MDRef MetadataNodeID
  | MDInline a
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

instance Functor MDRef where
  fmap _ (MDRef i) = MDRef i
  fmap f (MDInline a) = MDInline (f a)

data DWOpFragment = DW_OP_LLVM_Fragment
  { offset :: Word64
  , size :: Word64
  } deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <https://llvm.org/docs/LangRef.html#diexpression>
data DWOp
  = DW_OP_And
  | DW_OP_Bregx
  | DW_OP_ConstU Word64
  | DW_OP_Deref
  | DW_OP_Div
  | DW_OP_Dup
  | DwOpFragment DWOpFragment -- ^ Must appear at the end
  | DW_OP_Lit0
  | DW_OP_Minus
  | DW_OP_Mod
  | DW_OP_Mul
  | DW_OP_Not
  | DW_OP_Or
  | DW_OP_Plus
  | DW_OP_PlusUConst Word64
  | DW_OP_PushObjectAddress
  | DW_OP_Shl
  | DW_OP_Shr
  | DW_OP_Shra
  | DW_OP_StackValue -- ^ Must be the last one or followed by a DW_OP_LLVM_Fragment
  | DW_OP_Swap
  | DW_OP_XDeref
  | DW_OP_Xor
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <http://llvm.org/docs/LangRef.html#metadata>
data MDNode
  = MDTuple [Maybe Metadata] -- ^ Nothing represents 'null'
  | DIExpression DIExpression
  | DIGlobalVariableExpression DIGlobalVariableExpression
  | DILocation DILocation
  | DIMacroNode DIMacroNode
  | DINode DINode
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DILocation = Location
  { line :: Word32
  , column :: Word16
  , scope :: MDRef DILocalScope
  } deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <https://llvm.org/docs/LangRef.html#diexpression>
data DIExpression = Expression
  { operands :: [DWOp]
  } deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | A pair of a `DIGlobalVariable` and a `DIExpression`.
--
-- This is used in the `cuGlobals` fields of `DICompileUnit`.
data DIGlobalVariableExpression = GlobalVariableExpression
  { var :: MDRef DIGlobalVariable
  , expr :: MDRef DIExpression
  } deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | Accessiblity flag
data DIAccessibility
  = Private
  | Protected
  | Public
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | Inheritance flag
data DIInheritance
  = SingleInheritance
  | MultipleInheritance
  | VirtualInheritance
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DIFlag
  = Accessibility DIAccessibility
  | FwdDecl
  | AppleBlock
  | BlockByrefStruct
  | VirtualFlag
  | Artificial
  | Explicit
  | Prototyped
  | ObjcClassComplete
  | ObjectPointer
  | Vector
  | StaticMember
  | LValueReference
  | RValueReference
  | InheritanceFlag DIInheritance
  | IntroducedVirtual
  | BitField
  | NoReturn
  | ArgumentNotModified
  | TypePassByValue
  | TypePassByReference
  | EnumClass
  | Thunk
  | NonTrivial
  | BigEndian
  | LittleEndian
  | AllCallsDescribed
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DIMacroInfo = Define | Undef
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <https://llvm.org/doxygen/classllvm_1_1DIMacroNode.html>
data DIMacroNode
  -- | <https://llvm.org/docs/LangRef.html#dimacro>
  = DIMacro
    { info :: DIMacroInfo
    , line :: Word32
    , name :: ShortByteString
    , value :: ShortByteString
    }
  -- | <https://llvm.org/docs/LangRef.html#dimacrofile>
  | DIMacroFile
    { line :: Word32
    , file :: MDRef DIFile
    , elements :: [MDRef DIMacroNode]
    }
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <https://llvm.org/doxygen/classllvm_1_1DINode.html>
data DINode
  = DIEnumerator DIEnumerator
  | DIImportedEntity DIImportedEntity
  | DIObjCProperty DIObjCProperty
  | DIScope DIScope
  | DISubrange DISubrange
  | DITemplateParameter DITemplateParameter
  | DIVariable DIVariable
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <https://llvm.org/doxygen/classllvm_1_1DIObjCProperty.html>
data DIObjCProperty = ObjCProperty
  { name :: ShortByteString
  , file :: Maybe (MDRef DIFile)
  , line :: Word32
  , getterName :: ShortByteString
  , setterName :: ShortByteString
  , attributes :: Word32
  , type' :: Maybe (MDRef DIType)
  } deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data ImportedEntityTag = ImportedModule | ImportedDeclaration
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <https://llvm.org/doxygen/classllvm_1_1DIImportedEntity.html>
data DIImportedEntity = ImportedEntity
  { tag :: ImportedEntityTag
  , name :: ShortByteString
  , scope :: MDRef DIScope
  , entity :: Maybe (MDRef DINode)
  , file :: Maybe (MDRef DIFile)
  , line :: Word32
  } deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <https://llvm.org/docs/LangRef.html#dienumerator>
data DIEnumerator =
  Enumerator { value :: Int64, isUnsigned :: Bool, name :: ShortByteString }
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <https://llvm.org/docs/LangRef.html#disubrange>
data DISubrange =
  Subrange { count :: DICount, lowerBound :: Int64 }
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DICount
  = DICountConstant Int64
  | DICountVariable (MDRef DIVariable)
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <https://llvm.org/doxygen/classllvm_1_1DIScope.html>
data DIScope
  = DICompileUnit DICompileUnit
  | DIFile DIFile
  | DILocalScope DILocalScope
  | DIModule DIModule
  | DINamespace DINamespace
  | DIType DIType
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DIModule = Module
  { scope :: Maybe (MDRef DIScope)
  , name :: ShortByteString
  , configurationMacros :: ShortByteString
  , includePath :: ShortByteString
  , isysRoot :: ShortByteString
  } deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DINamespace = Namespace
  { name :: ShortByteString
  , scope :: Maybe (MDRef DIScope)
  , exportSymbols :: Bool
  } deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DebugEmissionKind = NoDebug | FullDebug | LineTablesOnly
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DebugNameTableKind = NameTableKindDefault | NameTableKindGNU | NameTableKindNone
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <https://llvm.org/docs/LangRef.html#dicompileunit>
data DICompileUnit = CompileUnit
  { language :: Word32
  , file :: MDRef DIFile
  , producer :: ShortByteString
  , optimized :: Bool
  , flags :: ShortByteString
  , runtimeVersion :: Word32
  , splitDebugFileName :: ShortByteString
  , emissionKind :: DebugEmissionKind
  , enums :: [MDRef DICompositeType] -- ^ Only enum types are allowed here
  , retainedTypes :: [MDRef (Either DIType DISubprogram)]
  , globals :: [MDRef DIGlobalVariableExpression]
  , imports :: [MDRef DIImportedEntity]
  , macros :: [MDRef DIMacroNode]
  , dWOId :: Word64
  , splitDebugInlining :: Bool
  , debugInfoForProfiling :: Bool
  , nameTableKind :: DebugNameTableKind
  , debugBaseAddress :: Bool
  } deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <https://llvm.org/docs/LangRef.html#difile>
data DIFile = File
  { filename :: ShortByteString
  , directory :: ShortByteString
  , checksum :: Maybe ChecksumInfo
  } deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data ChecksumInfo = ChecksumInfo
  { checksumKind :: ChecksumKind
  , checksumValue :: ShortByteString
  } deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data ChecksumKind = MD5 | SHA1
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <https://llvm.org/doxygen/classllvm_1_1DILocalScope.html>
data DILocalScope
  = DILexicalBlockBase DILexicalBlockBase
  | DISubprogram DISubprogram
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <https://llvm.org/docs/LangRef.html#disubprogram>
data DISubprogram = Subprogram
  { scope :: Maybe (MDRef DIScope)
  , name :: ShortByteString
  , linkageName :: ShortByteString
  , file :: Maybe (MDRef DIFile)
  , line :: Word32
  , type' :: Maybe (MDRef DISubroutineType)
  , localToUnit :: Bool
  , definition :: Bool
  , scopeLine :: Word32
  , containingType :: Maybe (MDRef DIType)
  , virtuality :: Virtuality
  , virtualityIndex :: Word32
  , thisAdjustment :: Int32
  , flags :: [DIFlag]
  , optimized :: Bool
  , unit :: Maybe (MDRef DICompileUnit)
  , templateParams :: [MDRef DITemplateParameter]
  , declaration :: Maybe (MDRef DISubprogram)
  , retainedNodes :: [MDRef DILocalVariable]
  , thrownTypes :: [MDRef DIType]
  } deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data Virtuality = NoVirtuality | Virtual | PureVirtual
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data BasicTypeTag = BaseType | UnspecifiedType
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- <https://llvm.org/doxygen/classllvm_1_1DIType.html>
data DIType
  = DIBasicType DIBasicType
  | DICompositeType DICompositeType
  | DIDerivedType DIDerivedType
  | DISubroutineType DISubroutineType
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <https://llvm.org/docs/LangRef.html#dibasictype>
data DIBasicType = BasicType
  { name :: ShortByteString
  , sizeInBits :: Word64
  , alignInBits :: Word32
  , encoding :: Maybe Encoding
  , tag :: BasicTypeTag
  , flags :: [DIFlag]
  } deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <https://llvm.org/docs/LangRef.html#disubroutinetype>
data DISubroutineType = SubroutineType
  { flags :: [DIFlag]
  , cc :: Word8
  , typeArray :: [Maybe (MDRef DIType)]
  -- ^ The first element is the return type, the following are the
  -- operand types. `Nothing` corresponds to @void@.
  } deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DerivedTypeTag
  = Typedef
  | PointerType
  | PtrToMemberType
  | ReferenceType
  | RValueReferenceType
  | ConstType
  | VolatileType
  | RestrictType
  | AtomicType
  | Member
  | Inheritance
  | Friend
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <https://llvm.org/docs/LangRef.html#diderivedtype>
data DIDerivedType =
  DerivedType
    { tag :: DerivedTypeTag
    , name :: ShortByteString
    , file :: Maybe (MDRef DIFile)
    , line :: Word32
    , scope :: Maybe (MDRef DIScope)
    , baseType :: Maybe (MDRef DIType)
    -- ^ This can be `Nothing` to represent @void *@
    , sizeInBits :: Word64
    , alignInBits :: Word32
    , offsetInBits :: Word64
    , addressSpace :: Maybe Word32
    , flags :: [DIFlag]
    } deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <https://llvm.org/docs/LangRef.html#dicompositetype>
data DICompositeType
  = DIArrayType
    { subscripts :: [DISubrange]
    , elementTy :: Maybe (MDRef DIType)
    , sizeInBits :: Word64
    , alignInBits :: Word32
    , flags :: [DIFlag]
    }
  | DIClassType
    { scope :: Maybe (MDRef DIScope)
    , name :: ShortByteString
    , file :: Maybe (MDRef DIFile)
    , line :: Word32
    , flags :: [DIFlag]
    , derivedFrom :: Maybe (MDRef DIType)
    , elements :: [MDRef (Either DIDerivedType DISubprogram)]
    -- ^ `DIDerivedType` with tag set to one of `Member`, `Inheritance`, `Friend`
    -- or `DISubprogram` with `definition` set to `True`.
    , vtableHolder :: Maybe (MDRef DIType)
    , templateParams :: [DITemplateParameter]
    , identifier :: ShortByteString
    , sizeInBits :: Word64
    , alignInBits :: Word32
    }
  | DIEnumerationType
    { scope :: Maybe (MDRef DIScope)
    , name :: ShortByteString
    , file :: Maybe (MDRef DIFile)
    , line :: Word32
    , values :: [DIEnumerator]
    , baseType :: Maybe (MDRef DIType)
    , identifier :: ShortByteString
    , sizeInBits :: Word64
    , alignInBits :: Word32
    }
  | DIStructureType
    { scope :: Maybe (MDRef DIScope)
    , name :: ShortByteString
    , file :: Maybe (MDRef DIFile)
    , line :: Word32
    , flags :: [DIFlag]
    , derivedFrom :: Maybe (MDRef DIType)
    , elements :: [MDRef (Either DIDerivedType DISubprogram)]
    -- ^ `DIDerivedType` with tag set to one of `Member`, `Inheritance`, `Friend`
    -- or `DISubprogram` with `definition` set to `True`.
    , runtimeLang :: Word16
    , vtableHolder :: Maybe (MDRef DIType)
    , identifier :: ShortByteString
    , sizeInBits :: Word64
    , alignInBits :: Word32
    }
  | DIUnionType
    { scope :: Maybe (MDRef DIScope)
    , name :: ShortByteString
    , file :: Maybe (MDRef DIFile)
    , line :: Word32
    , flags :: [DIFlag]
    , elements :: [MDRef (Either DIDerivedType DISubprogram)]
    -- ^ `DIDerivedType` with tag set to one of `Member`, `Inheritance`, `Friend`
    -- or `DISubprogram` with `definition` set to `True`.
    , runtimeLang :: Word16
    , identifier :: ShortByteString
    , sizeInBits :: Word64
    , alignInBits :: Word32
    }
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data Encoding
  = AddressEncoding
  | BooleanEncoding
  | FloatEncoding
  | SignedEncoding
  | SignedCharEncoding
  | UnsignedEncoding
  | UnsignedCharEncoding
  | UTFEncoding
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data TemplateValueParameterTag
  = TemplateValueParameter
  | GNUTemplateTemplateParam
  | GNUTemplateParameterPack
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <https://llvm.org/doxygen/classllvm_1_1DITemplateParameter.html>
data DITemplateParameter
  = DITemplateTypeParameter
    { name :: ShortByteString
    , type' :: Maybe (MDRef DIType)
    -- ^ For DITemplateTypeParameter this field is required,
    -- for DITemplateValueParameter it is optional.
    }
  -- ^ <https://llvm.org/docs/LangRef.html#ditemplatetypeparameter>
  | DITemplateValueParameter
    { name :: ShortByteString
    , type' :: Maybe (MDRef DIType)
    , value :: Maybe Metadata
    , tag :: TemplateValueParameterTag
    }
  -- ^ <https://llvm.org/docs/LangRef.html#ditemplatevalueparameter>
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <https://llvm.org/doxygen/classllvm_1_1DILexicalBlockBase.html>
data DILexicalBlockBase
  = DILexicalBlock
    { scope :: MDRef DILocalScope
    , file :: Maybe (MDRef DIFile)
    , line :: Word32
    , column :: Word16
    }
  -- ^ <https://llvm.org/docs/LangRef.html#dilexicalblock>
  | DILexicalBlockFile
    { scope :: MDRef DILocalScope
    , file :: Maybe (MDRef DIFile)
    , discriminator :: Word32
    }
  -- ^ <https://llvm.org/docs/LangRef.html#dilexicalblockfile>
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <https://llvm.org/doxygen/classllvm_1_1DIVariable.html>
data DIVariable
  = DIGlobalVariable DIGlobalVariable
  | DILocalVariable DILocalVariable
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <https://llvm.org/docs/LangRef.html#diglobalvariable>
data DIGlobalVariable = GlobalVariable
  { name :: ShortByteString
  , scope :: Maybe (MDRef DIScope)
  , file :: Maybe (MDRef DIFile)
  , line :: Word32
  , type' :: Maybe (MDRef DIType)
  , linkageName :: ShortByteString
  , local :: Bool
  , definition :: Bool
  , staticDataMemberDeclaration :: Maybe (MDRef DIDerivedType)
  , templateParams :: [MDRef DITemplateParameter]
  , alignInBits :: Word32
  } deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <https://llvm.org/docs/LangRef.html#dilocalvariable>
data DILocalVariable = LocalVariable
  { name :: ShortByteString
  , scope :: MDRef DIScope
  , file :: Maybe (MDRef DIFile)
  , line :: Word32
  , type' :: Maybe (MDRef DIType)
  , flags :: [DIFlag]
  , arg :: Word16
  , alignInBits :: Word32
  } deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
