{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LLVM.Test.Metadata where

import LLVM.Prelude

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck as QC

import LLVM.Test.Support

import Control.Monad.IO.Class
import Data.ByteString as B (readFile)
import qualified Data.ByteString.Short as BSS
import Data.Functor.Identity
import Data.Maybe (catMaybes)
import Foreign.Ptr
import Text.Show.Pretty (pPrint)

import qualified LLVM.AST.IntegerPredicate as IP
import LLVM.AST as A hiding (GlobalVariable, PointerType)
import LLVM.AST.Operand hiding (Module)
import qualified LLVM.AST.Operand as O
import qualified LLVM.AST.Operand as A (DIFlag(..), Virtuality(..), DIInheritance(..), DIAccessibility(..))
import LLVM.AST.Type as A.T hiding (PointerType)
import LLVM.AST.AddrSpace as A
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Visibility as V
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import LLVM.AST.Global as G hiding (GlobalVariable)

import LLVM.Context
import LLVM.Module
import LLVM.Internal.Coding
import LLVM.Internal.DecodeAST
import LLVM.Internal.EncodeAST
import qualified LLVM.Internal.FFI.PtrHierarchy as FFI
import qualified LLVM.Internal.FFI.Metadata as FFI

tests = testGroup "Metadata"
  [ globalMetadata
  , namedMetadata
  , nullMetadata
  , cyclicMetadata
  , metadataConstantFolding
  , globalObjectMetadata
  , roundtripDIBasicType
  , roundtripDIDerivedType
  , roundtripDISubroutineType
  , roundtripDIArrayType
  , roundtripDIEnumerationType
  , roundtripDIStructureType
  , roundtripDIClassType
  , roundtripDIUnionType
  , roundtripDIFile
  , roundtripDINode
  , roundtripDICompileUnit
  , roundtripDIVariable
  , roundtripDIFlags
  , roundtripDISubprogram
  , roundtripDILexicalBlockBase
  , roundtripDITemplateParameter
  , roundtripDINamespace
  , roundtripDIExpression
  , diFlagName
  , testFile
  ]

instance Arbitrary Encoding where
  arbitrary =
    QC.elements
      [ AddressEncoding
      , BooleanEncoding
      , FloatEncoding
      , SignedEncoding
      , SignedCharEncoding
      , UnsignedEncoding
      , UnsignedCharEncoding
      , UTFEncoding
      ]

instance Arbitrary ChecksumInfo where
  arbitrary =
    oneof
      [ ChecksumInfo MD5 . BSS.pack <$> QC.vector 32
      , ChecksumInfo SHA1 . BSS.pack <$> QC.vector 40
      ]

instance Arbitrary BasicTypeTag where
  arbitrary = QC.elements [BaseType, UnspecifiedType]

instance Arbitrary DIType where
  arbitrary = oneof [DIBasicType <$> arbitrary]

instance Arbitrary DIBasicType where
  arbitrary = BasicType <$> arbitrarySbs <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> genDIFlags

genDIArrayType :: Maybe (MDRef DIType) -> Gen DICompositeType
genDIArrayType elTy =
  DIArrayType <$> arbitrary <*> pure elTy <*> arbitrary <*> arbitrary <*> genDIFlags

instance Arbitrary DIMacroInfo where
  arbitrary = QC.elements [Define, Undef]

genDIMacro :: Gen DIMacroNode
genDIMacro =
  DIMacro <$> arbitrary <*> arbitrary <*> arbitrarySbs <*> arbitrarySbs

roundtripDIArrayType :: TestTree
roundtripDIArrayType = testProperty "roundtrip DIArrayType" $ \elType ->
  forAll (genDIArrayType (Just (MDRef elTyID))) $ \diArrayType -> ioProperty $
    withContext $ \context -> runEncodeAST context $ do
      let mod = defaultModule
            { moduleDefinitions =
                [ NamedMetadataDefinition "dummy" [arrayTypeID]
                , MetadataNodeDefinition arrayTypeID (DINode (DIScope (DIType (DICompositeType diArrayType))))
                , MetadataNodeDefinition elTyID (DINode (DIScope (DIType elType)))
                ]
            }
      mod' <- liftIO (withModuleFromAST context mod moduleAST)
      pure (mod' === mod)
  where arrayTypeID = MetadataNodeID 0
        elTyID = MetadataNodeID 1

genDIEnumerationType :: Maybe (MDRef DIScope) -> Maybe (MDRef DIFile) -> Maybe (MDRef DIType) -> Gen DICompositeType
genDIEnumerationType scope file baseTy =
  DIEnumerationType
    <$> pure scope
    <*> arbitrarySbs
    <*> pure file
    <*> arbitrary
    <*> arbitrary
    <*> pure baseTy
    <*> arbitrarySbs
    <*> arbitrary
    <*> arbitrary

roundtripDIEnumerationType :: TestTree
roundtripDIEnumerationType = testProperty "roundtrip DIEnumerationType" $ \file baseType ->
  forAll (genDIEnumerationType Nothing (Just (MDRef fileID)) (Just (MDRef baseTyID))) $ \diEnumType -> ioProperty $
    withContext $ \context -> runEncodeAST context $ do
      let mod = defaultModule
            { moduleDefinitions =
                [ NamedMetadataDefinition "dummy" [enumTypeID]
                , MetadataNodeDefinition enumTypeID (DINode (DIScope (DIType (DICompositeType diEnumType))))
                , MetadataNodeDefinition fileID (DINode (DIScope (DIFile file)))
                , MetadataNodeDefinition baseTyID (DINode (DIScope (DIType baseType)))
                ]
            }
      mod' <- liftIO (withModuleFromAST context mod moduleAST)
      pure (mod' === mod)
  where enumTypeID = MetadataNodeID 0
        fileID = MetadataNodeID 1
        baseTyID = MetadataNodeID 2

genDIStructureType :: Maybe (MDRef DIScope) -> Maybe (MDRef DIFile) -> Maybe (MDRef DIType) -> [MDRef (Either DIDerivedType DISubprogram)] -> Maybe (MDRef DIType) -> Gen DICompositeType
genDIStructureType scope file derivedFrom elements vtableHolder =
  DIStructureType
    <$> pure scope
    <*> arbitrarySbs
    <*> pure file
    <*> arbitrary
    <*> genDIFlags
    <*> pure derivedFrom
    <*> pure elements
    <*> arbitrary
    <*> pure vtableHolder
    <*> arbitrarySbs
    <*> arbitrary
    <*> arbitrary

roundtripDIStructureType :: TestTree
roundtripDIStructureType = testProperty "roundtrip DIStructureType" $
  forAll (genDIStructureType Nothing Nothing Nothing [] Nothing) $ \diStructureType -> ioProperty $
    withContext $ \context -> runEncodeAST context $ do
      let mod = defaultModule
            { moduleDefinitions =
                [ NamedMetadataDefinition "dummy" [structureTypeID]
                , MetadataNodeDefinition structureTypeID (DINode (DIScope (DIType (DICompositeType diStructureType))))
                ]
            }
      mod' <- liftIO (withModuleFromAST context mod moduleAST)
      pure (mod' === mod)
  where structureTypeID = MetadataNodeID 0

genDIClassType :: Maybe (MDRef DIScope) -> Maybe (MDRef DIFile) -> Maybe (MDRef DIType) -> [MDRef (Either DIDerivedType DISubprogram)] -> Maybe (MDRef DIType) -> [DITemplateParameter] -> Gen DICompositeType
genDIClassType scope file derivedFrom elements vtableHolder templateParams =
  DIClassType
    <$> pure scope
    <*> arbitrarySbs
    <*> pure file
    <*> arbitrary
    <*> genDIFlags
    <*> pure derivedFrom
    <*> pure elements
    <*> pure vtableHolder
    <*> pure templateParams
    <*> arbitrarySbs
    <*> arbitrary
    <*> arbitrary

roundtripDIClassType :: TestTree
roundtripDIClassType = testProperty "roundtrip DIClassType" $
  forAll (genDIClassType Nothing Nothing Nothing [] Nothing []) $ \diClassType -> ioProperty $
    withContext $ \context -> runEncodeAST context $ do
      let mod = defaultModule
            { moduleDefinitions =
                [ NamedMetadataDefinition "dummy" [classTypeID]
                , MetadataNodeDefinition classTypeID (DINode (DIScope (DIType (DICompositeType diClassType))))
                ]
            }
      mod' <- liftIO (withModuleFromAST context mod moduleAST)
      pure (mod' === mod)
  where classTypeID = MetadataNodeID 0

genDIUnionType :: Maybe (MDRef DIScope) -> Maybe (MDRef DIFile) -> [MDRef (Either DIDerivedType DISubprogram)] -> Gen DICompositeType
genDIUnionType scope file elements =
  DIUnionType
    <$> pure scope
    <*> arbitrarySbs
    <*> pure file
    <*> arbitrary
    <*> genDIFlags
    <*> pure elements
    <*> arbitrary
    <*> arbitrarySbs
    <*> arbitrary
    <*> arbitrary

roundtripDIUnionType :: TestTree
roundtripDIUnionType = testProperty "roundtrip DIUnionType" $
  forAll (genDIUnionType Nothing Nothing []) $ \diUnionType -> ioProperty $
    withContext $ \context -> runEncodeAST context $ do
      let mod = defaultModule
            { moduleDefinitions =
                [ NamedMetadataDefinition "dummy" [unionTypeID]
                , MetadataNodeDefinition unionTypeID (DINode (DIScope (DIType (DICompositeType diUnionType))))
                ]
            }
      mod' <- liftIO (withModuleFromAST context mod moduleAST)
      pure (mod' === mod)
  where unionTypeID = MetadataNodeID 0

instance Arbitrary DIFile where
  arbitrary =
    O.File <$> arbitrarySbs <*> arbitrarySbs <*> arbitrary

instance Arbitrary DISubrange where
  arbitrary = Subrange <$> arbitrary <*> arbitrary

instance Arbitrary DICount where
  -- TODO Include DICountVariable
  arbitrary = DICountConstant <$> arbitrary

instance Arbitrary DIEnumerator where
  arbitrary = Enumerator <$> arbitrary <*> arbitrary <*> arbitrarySbs

instance Arbitrary DINode where
  arbitrary =
    oneof
      [ DISubrange <$> arbitrary
      , DIEnumerator <$> arbitrary
      -- TODO: Add missing constructors
      ]

roundtripDIBasicType :: TestTree
roundtripDIBasicType = testProperty "roundtrip DIBasicType" $ \diType -> ioProperty $
  withContext $ \context -> runEncodeAST context $ do
    encodedDIType <- encodeM (diType :: DIType)
    decodedDIType <- liftIO (runDecodeAST (decodeM (encodedDIType :: Ptr FFI.DIType)))
    pure (decodedDIType === diType)

roundtripDIDerivedType :: TestTree
roundtripDIDerivedType = testProperty "roundtrip DIDerivedType" $ \baseType ->
  forAll (QC.elements [Nothing, Just (MDRef baseTypeID)]) $ \baseType' ->
  forAll (genDIDerivedType Nothing Nothing baseType') $ \diDerivedType -> ioProperty $
    withContext $ \context -> runEncodeAST context $ do
      let mod = defaultModule
            { moduleDefinitions =
                [ NamedMetadataDefinition "dummy" [derivedTypeID]
                , NamedMetadataDefinition "dummy2" [baseTypeID]
                , MetadataNodeDefinition derivedTypeID (DINode (DIScope (DIType (DIDerivedType diDerivedType))))
                , MetadataNodeDefinition baseTypeID (DINode (DIScope (DIType baseType)))
                ]
            }
      mod' <- liftIO (withModuleFromAST context mod moduleAST)
      pure (mod' === mod)
  where derivedTypeID = MetadataNodeID 0
        baseTypeID = MetadataNodeID 1

instance Arbitrary DerivedTypeTag where
  arbitrary =
    QC.elements [ Typedef, PointerType, PtrToMemberType, ReferenceType, RValueReferenceType
             , ConstType, VolatileType, RestrictType, AtomicType, Member, Inheritance, Friend
             ]

genDIDerivedType :: Maybe (MDRef DIFile) -> Maybe (MDRef DIScope) -> Maybe (MDRef DIType) -> Gen DIDerivedType
genDIDerivedType file scope baseType =
  DerivedType
    <$> arbitrary
    <*> arbitrarySbs
    <*> pure file
    <*> arbitrary
    <*> pure scope
    <*> pure baseType
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> genDIFlags

roundtripDISubroutineType :: TestTree
roundtripDISubroutineType = testProperty "roundtrip DISubroutineType" $ \argType ->
  forAll (genDISubroutineType [Nothing, Just (MDRef argTypeID)]) $ \diSubroutineType -> ioProperty $
    withContext $ \context -> runEncodeAST context $ do
      let mod = defaultModule
            { moduleDefinitions =
                [ NamedMetadataDefinition "dummy" [subroutineTypeID]
                , MetadataNodeDefinition subroutineTypeID (DINode (DIScope (DIType (DISubroutineType diSubroutineType))))
                , MetadataNodeDefinition argTypeID (DINode (DIScope (DIType argType)))
                ]
            }
      mod' <- liftIO (withModuleFromAST context mod moduleAST)
      pure (mod' === mod)
  where subroutineTypeID = MetadataNodeID 0
        argTypeID = MetadataNodeID 1

genDISubroutineType :: [Maybe (MDRef DIType)] -> Gen DISubroutineType
genDISubroutineType types =
  SubroutineType
    <$> genDIFlags
    <*> arbitrary
    <*> pure types

roundtripDIFile :: TestTree
roundtripDIFile = testProperty "roundtrip DIFile" $ \diFile -> ioProperty $
  withContext $ \context -> runEncodeAST context $ do
    encodedDIFile <- encodeM (diFile :: DIFile)
    decodedDIFile <- liftIO (runDecodeAST (decodeM (encodedDIFile :: Ptr FFI.DIFile)))
    pure (decodedDIFile === diFile)

roundtripDINode :: TestTree
roundtripDINode = testProperty "roundtrip DINode" $ \diNode -> ioProperty $
  withContext $ \context -> runEncodeAST context $ do
    encodedDINode <- encodeM (diNode :: DINode)
    decodedDINode :: Either String DINode <- liftIO (runDecodeAST (decodeM (encodedDINode :: Ptr FFI.DINode)))
    pure (decodedDINode === (Right diNode))

roundtripDICompileUnit :: TestTree
roundtripDICompileUnit = testProperty "roundtrip DICompileUnit" $ \diFile retainedType ->
  forAll genDIMacro $ \diMacro ->
  forAll (genDICompileUnit (MDRef fileID) (MDRef retainedID) (MDRef macroID)) $ \diCompileUnit -> ioProperty $
    withContext $ \context -> runEncodeAST context $ do
      let mod = defaultModule
            { moduleDefinitions =
                [ NamedMetadataDefinition "dummy" [cuID]
                , NamedMetadataDefinition "dummyMacro" [macroID]
                , NamedMetadataDefinition "dummyRetained" [retainedID]
                , MetadataNodeDefinition cuID (DINode (DIScope (DICompileUnit diCompileUnit)))
                , MetadataNodeDefinition macroID (DIMacroNode diMacro)
                , MetadataNodeDefinition retainedID (DINode (DIScope (DIType retainedType)))
                , MetadataNodeDefinition fileID (DINode (DIScope (DIFile diFile)))
                ]
            }
      mod' <- liftIO (withModuleFromAST context mod moduleAST)
      pure (mod' === mod)
  where cuID = MetadataNodeID 0
        macroID = MetadataNodeID 1
        retainedID = MetadataNodeID 2
        fileID = MetadataNodeID 3

genDICompileUnit :: MDRef DIFile -> MDRef (Either DIType DISubprogram) -> MDRef DIMacroNode -> Gen DICompileUnit
genDICompileUnit file retained macro =
  CompileUnit
    <$> arbitrary
    <*> pure file
    <*> arbitrarySbs
    <*> arbitrary
    <*> arbitrarySbs
    <*> arbitrary
    <*> arbitrarySbs
    <*> arbitrary
    <*> pure []
    <*> listOf (pure retained)
    <*> pure []
    <*> pure []
    <*> listOf (pure macro)
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary DebugEmissionKind where
  arbitrary = QC.elements [NoDebug, FullDebug, LineTablesOnly]

instance Arbitrary DebugNameTableKind where
  arbitrary = QC.elements [NameTableKindDefault, NameTableKindGNU, NameTableKindNone]

roundtripDIVariable :: TestTree
roundtripDIVariable = testProperty "roundtrip DIVariable" $ \diFile diType ->
  forAll (genDIVariable Nothing (Just (MDRef fileID)) (Just (MDRef typeID))) $ \diVariable -> ioProperty $
    withContext $ \context -> runEncodeAST context $ do
      let mod = defaultModule
            { moduleDefinitions =
                [ NamedMetadataDefinition "dummy" [varID]
                , MetadataNodeDefinition varID (DINode (DIVariable diVariable))
                , MetadataNodeDefinition fileID (DINode (DIScope (DIFile diFile)))
                , MetadataNodeDefinition typeID (DINode (DIScope (DIType diType)))
                ]
            }
      mod' <- liftIO (withModuleFromAST context mod moduleAST)
      pure (mod' === mod)
  where varID = MetadataNodeID 0
        fileID = MetadataNodeID 1
        typeID = MetadataNodeID 2

genDIVariable :: Maybe (MDRef DIScope) -> Maybe (MDRef DIFile) -> Maybe (MDRef DIType) -> Gen DIVariable
genDIVariable diScope diFile diType =
  case diScope of
    Nothing -> DIGlobalVariable <$> globalVar
    Just scope -> oneof [DILocalVariable <$> localVar scope, DIGlobalVariable <$> globalVar]
  where
    localVar scope =
      LocalVariable
        <$> arbitrarySbs
        <*> pure scope
        <*> pure diFile
        <*> arbitrary
        <*> pure diType
        <*> genDIFlags
        <*> arbitrary
        <*> arbitrary
    globalVar =
      GlobalVariable
        <$> arbitrarySbs
        <*> pure diScope
        <*> pure diFile
        <*> arbitrary
        <*> pure diType
        <*> arbitrarySbs
        <*> arbitrary
        <*> arbitrary
        <*> pure Nothing
        <*> pure []
        <*> arbitrary

instance Arbitrary A.DIInheritance where
  arbitrary = QC.elements [A.SingleInheritance, A.MultipleInheritance, A.VirtualInheritance]

instance Arbitrary A.DIAccessibility where
  arbitrary = QC.elements [A.Public, A.Protected, A.Private]

instance Arbitrary A.DIFlag where
  arbitrary =
    oneof
      [ A.Accessibility <$> arbitrary
      , A.InheritanceFlag <$> arbitrary
      , QC.elements
          [ A.FwdDecl
          , A.AppleBlock
          , A.BlockByrefStruct
          , A.VirtualFlag
          , A.Artificial
          , A.Explicit
          , A.Prototyped
          , A.ObjcClassComplete
          , A.ObjectPointer
          , A.Vector
          , A.StaticMember
          , A.LValueReference
          , A.RValueReference
          , A.IntroducedVirtual
          , A.BitField
          , A.NoReturn
          , A.ArgumentNotModified
          , A.TypePassByValue
          , A.TypePassByReference
          , A.EnumClass
          , A.Thunk
          , A.NonTrivial
          , A.BigEndian
          , A.LittleEndian
          , A.AllCallsDescribed
          ]
      ]

roundtripDIFlags :: TestTree
roundtripDIFlags =
  testProperty "roundtrip DIFlags" $
  forAll genDIFlags $ \diFlags ->
    let Identity encodedFlags = encodeM diFlags
        Identity decodedFlags = decodeM (encodedFlags :: FFI.DIFlags)
    in decodedFlags === diFlags

genDIFlags :: Gen [DIFlag]
genDIFlags = do
  accessibility <-
    QC.elements [Nothing, Just A.Public, Just A.Protected, Just A.Private]
  inheritance <-
    QC.elements
      [ Nothing
      , Just A.SingleInheritance
      , Just A.MultipleInheritance
      , Just A.VirtualInheritance
      ]
  maybeFlags <- traverse (\f -> QC.elements [Nothing, Just f]) flags
  pure
    (catMaybes
       ((A.Accessibility <$> accessibility) :
        (A.InheritanceFlag <$> inheritance) : maybeFlags))
  where
    flags =
      [ A.FwdDecl
      , A.AppleBlock
      , A.BlockByrefStruct
      , A.VirtualFlag
      , A.Artificial
      , A.Explicit
      , A.Prototyped
      , A.ObjcClassComplete
      , A.ObjectPointer
      , A.Vector
      , A.StaticMember
      , A.LValueReference
      , A.RValueReference
      , A.IntroducedVirtual
      , A.BitField
      , A.NoReturn
      , A.ArgumentNotModified
      , A.TypePassByValue
      , A.TypePassByReference
      , A.EnumClass
      , A.Thunk
      , A.NonTrivial
      , A.BigEndian
      , A.LittleEndian
      , A.AllCallsDescribed
      ]

roundtripDISubprogram :: TestTree
roundtripDISubprogram = testProperty "roundtrip DISubprogram" $
  forAll (genDISubprogram Nothing Nothing Nothing Nothing Nothing [] Nothing [] []) $ \diSubprogram -> ioProperty $
    withContext $ \context -> runEncodeAST context $ do
      let mod = defaultModule
            { moduleDefinitions =
                [ NamedMetadataDefinition "dummy" [subprogramID]
                , MetadataNodeDefinition subprogramID (DINode (DIScope (DILocalScope (DISubprogram diSubprogram))))
                ]
            }
      mod' <- liftIO (withModuleFromAST context mod moduleAST)
      pure (mod' === mod)
  where subprogramID = MetadataNodeID 0

genDISubprogram :: Maybe (MDRef DIScope) -> Maybe (MDRef DIFile) -> Maybe (MDRef DISubroutineType) ->
                   Maybe (MDRef DIType) -> Maybe (MDRef DICompileUnit) -> [MDRef DITemplateParameter] ->
                   Maybe (MDRef DISubprogram) ->
                   [MDRef DILocalVariable] -> [MDRef DIType] -> Gen DISubprogram
genDISubprogram scope file type' containingType unit templateParams decl vars thrownTypes =
  Subprogram
    <$> pure scope
    <*> arbitrarySbs
    <*> arbitrarySbs
    <*> pure file
    <*> arbitrary
    <*> pure type'
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> pure containingType
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> genDIFlags
    <*> arbitrary
    <*> pure unit
    <*> pure templateParams
    <*> pure decl
    <*> pure vars
    <*> pure thrownTypes

roundtripDILexicalBlockBase :: TestTree
roundtripDILexicalBlockBase = testProperty "roundtrip DILexicalBlockBase" $ \diFile ->
  forAll (genDISubprogram Nothing Nothing Nothing Nothing Nothing [] Nothing [] []) $ \diSubprogram ->
  forAll (genDILexicalBlockBase (MDRef subprogramID) (Just (MDRef fileID))) $ \block -> ioProperty $
    withContext $ \context -> runEncodeAST context $ do
      let mod = defaultModule
            { moduleDefinitions =
                [ NamedMetadataDefinition "dummy" [blockID]
                , MetadataNodeDefinition blockID (DINode (DIScope (DILocalScope (DILexicalBlockBase block))))
                , MetadataNodeDefinition subprogramID (DINode (DIScope (DILocalScope (DISubprogram diSubprogram))))
                , MetadataNodeDefinition fileID (DINode (DIScope (DIFile diFile)))
                ]
            }
      mod' <- liftIO (withModuleFromAST context mod moduleAST)
      pure (mod' === mod)
  where blockID = MetadataNodeID 0
        subprogramID = MetadataNodeID 1
        fileID = MetadataNodeID 2

genDILexicalBlockBase :: MDRef DILocalScope -> Maybe (MDRef DIFile) -> Gen DILexicalBlockBase
genDILexicalBlockBase scope file =
  oneof
    [ DILexicalBlock scope file <$> arbitrary <*> arbitrary
    , DILexicalBlockFile scope file <$> arbitrary
    ]

instance Arbitrary Virtuality where
  arbitrary = QC.elements [A.NoVirtuality, A.Virtual, A.PureVirtual]

roundtripDITemplateParameter :: TestTree
roundtripDITemplateParameter = testProperty "rountrip DITemplateParameter" $ \diType ->
  forAll (genDITemplateParameter (MDValue (ConstantOperand (C.Int 32 1))) (MDRef tyID)) $ \param -> ioProperty $
    withContext $ \context -> runEncodeAST context $ do
      let mod = defaultModule
            { moduleDefinitions =
                [ NamedMetadataDefinition "dummy" [paramID]
                , NamedMetadataDefinition "dummyTy" [tyID]
                , MetadataNodeDefinition paramID (DINode (DITemplateParameter param))
                , MetadataNodeDefinition tyID (DINode (DIScope (DIType diType)))
                ]
            }
      mod' <- liftIO (withModuleFromAST context mod moduleAST)
      pure (mod' === mod)
  where paramID = MetadataNodeID 0
        tyID = MetadataNodeID 1

instance Arbitrary TemplateValueParameterTag where
  arbitrary = QC.elements [TemplateValueParameter, GNUTemplateTemplateParam, GNUTemplateParameterPack]

genDITemplateParameter :: Metadata -> MDRef DIType -> Gen DITemplateParameter
genDITemplateParameter value ty =
  oneof [ DITemplateTypeParameter <$> arbitrarySbs <*> pure (Just ty)
        , DITemplateValueParameter <$> arbitrarySbs <*> pure Nothing <*> pure (Just value) <*> arbitrary
        ]

roundtripDINamespace :: TestTree
roundtripDINamespace = testProperty "rountrip DINamespace" $ \diFile ->
  forAll (genDINamespace (MDRef fileID)) $ \diNamespace -> ioProperty $
    withContext $ \context -> runEncodeAST context $ do
      let mod = defaultModule
            { moduleDefinitions =
                [ NamedMetadataDefinition "dummy" [namespaceID]
                , NamedMetadataDefinition "dummy2" [fileID]
                , MetadataNodeDefinition namespaceID (DINode (DIScope (DINamespace diNamespace)))
                , MetadataNodeDefinition fileID (DINode (DIScope (DIFile diFile)))
                ]
            }
      mod' <- liftIO (withModuleFromAST context mod moduleAST)
      pure (mod' === mod)
  where namespaceID = MetadataNodeID 0
        fileID = MetadataNodeID 1

genDINamespace :: MDRef DIScope -> Gen DINamespace
genDINamespace scope = Namespace <$> arbitrarySbs <*> QC.elements [Nothing, Just scope] <*> arbitrary

diFlagName :: TestTree
diFlagName =
  testProperty "LLVM and llvm-hs agree on encoding of DIFlag" $ \diFlag ->
    ioProperty $
    withContext $ \context ->
      runEncodeAST context $ do
        encoded <- encodeM [diFlag]
        flagName' <- encodeM ("DIFlag" <> flagName diFlag)
        encodedByName <- liftIO (FFI.getDIFlag flagName')
        pure (encoded === encodedByName)
  where
    flagName (A.Accessibility f) = show f
    flagName (A.InheritanceFlag f) = show f
    flagName A.VirtualFlag = "Virtual"
    flagName f = show f

roundtripDIExpression :: TestTree
roundtripDIExpression = testProperty "roundtrip DIExpression" $
  forAll genDIExpression $ \expr -> ioProperty $
    withContext $ \context -> runEncodeAST context $ do
      encodedExpr <- encodeM expr
      decodedExpr <- liftIO (runDecodeAST (decodeM (encodedExpr :: Ptr FFI.DIExpression)))
      pure (decodedExpr === expr)

genDIExpression :: Gen DIExpression
genDIExpression = Expression <$> arbitrary

instance Arbitrary DWOp where
  arbitrary =
    oneof
      [ DwOpFragment <$> (DW_OP_LLVM_Fragment <$> arbitrary <*> arbitrary)
      , pure DW_OP_StackValue
      , pure DW_OP_Swap
      , pure DW_OP_Lit0
      , DW_OP_ConstU <$> arbitrary
      , DW_OP_PlusUConst <$> arbitrary
      , pure DW_OP_Plus
      , pure DW_OP_Minus
      , pure DW_OP_Mul
      , pure DW_OP_Div
      , pure DW_OP_Mod
      , pure DW_OP_Not
      , pure DW_OP_Or
      , pure DW_OP_Xor
      , pure DW_OP_And
      , pure DW_OP_Shr
      , pure DW_OP_Shra
      , pure DW_OP_Shl
      , pure DW_OP_Dup
      , pure DW_OP_Deref
      , pure DW_OP_XDeref
      ]

testFile :: TestTree
testFile = do
  testGroup "file parsing and decoding"
    [ testCase "test/debug_metadata_1.ll" $ do
        fStr <- B.readFile "test/debug_metadata_1.ll"
        withContext $ \context -> do
          a <- withModuleFromLLVMAssembly' context fStr moduleAST
          pure ()
    ,  testCase "test/debug_metadata_2.ll" $ do
         fStr <- B.readFile "test/debug_metadata_2.ll"
         withContext $ \context -> do
           a <- withModuleFromLLVMAssembly' context fStr moduleAST
           pure ()
    ,  testCase "test/debug_metadata_3.ll" $ do
         fStr <- B.readFile "test/debug_metadata_3.ll"
         withContext $ \context -> do
           a <- withModuleFromLLVMAssembly' context fStr moduleAST
           pure ()
    ,  testCase "test/debug_metadata_4.ll" $ do
         fStr <- B.readFile "test/debug_metadata_4.ll"
         withContext $ \context -> do
           a <- withModuleFromLLVMAssembly' context fStr moduleAST
           pure ()
    ,  testCase "test/debug_metadata_5.ll" $ do
         fStr <- B.readFile "test/debug_metadata_5.ll"
         withContext $ \context -> do
           a <- withModuleFromLLVMAssembly' context fStr moduleAST
           pure ()
    ]

globalMetadata = testCase "global" $ do
    let ast = Module "<string>" "<string>" Nothing Nothing [
          GlobalDefinition $ functionDefaults {
            G.returnType = i32,
            G.name = Name "foo",
            G.basicBlocks = [
              BasicBlock (UnName 0) [
              ] (
                Do $ Ret (Just (ConstantOperand (C.Int 32 0))) [
                  ("my-metadatum", A.MDRef (MetadataNodeID 0))
                ]
              )
             ]
            },
          MetadataNodeDefinition (MetadataNodeID 0) (MDTuple [ Just $ MDValue $ ConstantOperand (C.Int 32 1) ])
         ]
    let s = "; ModuleID = '<string>'\n\
            \source_filename = \"<string>\"\n\
            \\n\
            \define i32 @foo() {\n\
            \  ret i32 0, !my-metadatum !0\n\
            \}\n\
            \\n\
            \!0 = !{i32 1}\n"
    strCheck ast s

namedMetadata = testCase "named" $ do
    let ast = Module "<string>" "<string>" Nothing Nothing [
          NamedMetadataDefinition "my-module-metadata" [ MetadataNodeID 0 ],
          MetadataNodeDefinition (MetadataNodeID 0) (MDTuple [ Just $ MDValue $ ConstantOperand (C.Int 32 1) ])
         ]
    let s = "; ModuleID = '<string>'\n\
            \source_filename = \"<string>\"\n\
            \\n\
            \!my-module-metadata = !{!0}\n\
            \\n\
            \!0 = !{i32 1}\n"
    strCheck ast s

nullMetadata = testCase "null" $ do
    let ast = Module "<string>" "<string>" Nothing Nothing [
          NamedMetadataDefinition "my-module-metadata" [ MetadataNodeID 0 ],
          MetadataNodeDefinition (MetadataNodeID 0) (MDTuple [ Nothing ])
         ]
    let s = "; ModuleID = '<string>'\n\
            \source_filename = \"<string>\"\n\
            \\n\
            \!my-module-metadata = !{!0}\n\
            \\n\
            \!0 = !{null}\n"
    strCheck ast s

cyclicMetadata = testGroup "cyclic" [
    testCase "metadata-only" $ do
      let ast = Module "<string>" "<string>" Nothing Nothing [
            NamedMetadataDefinition "my-module-metadata" [MetadataNodeID 0],
            MetadataNodeDefinition
              (MetadataNodeID 0)
              (MDTuple [Just $ MDNode (MDRef (MetadataNodeID 1))]),
            MetadataNodeDefinition
              (MetadataNodeID 1)
              (MDTuple [Just $ MDNode (MDRef (MetadataNodeID 0))])
           ]
      let s = "; ModuleID = '<string>'\n\
              \source_filename = \"<string>\"\n\
              \\n\
              \!my-module-metadata = !{!0}\n\
              \\n\
              \!0 = !{!1}\n\
              \!1 = !{!0}\n"
      strCheck ast s,

    testCase "metadata-global" $ do
      let ast = Module "<string>" "<string>" Nothing Nothing [
            GlobalDefinition $ functionDefaults {
              G.returnType = A.T.void,
              G.name = Name "foo",
              G.basicBlocks = [
                BasicBlock (UnName 0) [
                 ] (
                   Do $ Ret Nothing [ ("my-metadatum", MDRef (MetadataNodeID 0)) ]
                 )
               ]
             },
            MetadataNodeDefinition
              (MetadataNodeID 0)
              (MDTuple [Just $ MDValue $ ConstantOperand (C.GlobalReference (ptr (FunctionType A.T.void [] False)) (Name "foo"))])
           ]
      let s = "; ModuleID = '<string>'\n\
              \source_filename = \"<string>\"\n\
              \\n\
              \define void @foo() {\n\
              \  ret void, !my-metadatum !0\n\
              \}\n\
              \\n\
              \!0 = !{void ()* @foo}\n"
      strCheck ast s
   ]

metadataConstantFolding = testGroup "constant folding" [
    testCase "metadata on instructions that can be constant folded" $
      let ast = Module "<string>" "<string>" Nothing Nothing
            [ GlobalDefinition functionDefaults
                { name = "f"
                , parameters = ([Parameter i32 "x" []] , False)
                , returnType = i32
                , basicBlocks =
                    [ BasicBlock "if"
                        [UnName 0 := ICmp IP.EQ (ConstantOperand (C.Int 32 0)) (ConstantOperand (C.Int 32 0))
                           [("foobar", MDRef (MetadataNodeID 0))]
                        ]
                        (Do (CondBr (LocalReference i1 (UnName 0)) "if.true" "if.false" []))
                    , BasicBlock "if.true" []
                        (Do (Ret (Just (ConstantOperand (C.Int 32 0))) []))
                    , BasicBlock "if.false" []
                        (Do (Ret (Just (ConstantOperand (C.Int 32 0))) []))
                    ]
                }
            , MetadataNodeDefinition (MetadataNodeID 0) (MDTuple [])
            ]
          s = "; ModuleID = '<string>'\n\
              \source_filename = \"<string>\"\n\
              \\n\
              \define i32 @f(i32 %x) {\n\
              \if:\n\
              \  %0 = icmp eq i32 0, 0, !foobar !0\n\
              \  br i1 %0, label %if.true, label %if.false\n\
              \\n\
              \if.true:                                          ; preds = %if\n\
              \  ret i32 0\n\
              \\n\
              \if.false:                                         ; preds = %if\n\
              \  ret i32 0\n\
              \}\n\
              \\n\
              \!0 = !{}\n"
      in strCheck ast s
    ]

globalObjectMetadata = testGroup "Metadata on GlobalObject" $
  [ testCase "metadata on functions" $ do
      let ast = Module "<string>" "<string>" Nothing Nothing
                  [ GlobalDefinition
                      functionDefaults
                        { G.name = "main"
                        , G.returnType = A.T.void
                        , basicBlocks = [ BasicBlock (UnName 0) [] (Do (Ret Nothing [])) ]
                        , G.metadata = [("dbg", MDRef (MetadataNodeID 0))]
                        }
                  , NamedMetadataDefinition "llvm.module.flags" [MetadataNodeID 1]
                  , NamedMetadataDefinition "llvm.dbg.cu" [MetadataNodeID 2]
                  , MetadataNodeDefinition (MetadataNodeID 0) $
                    DINode .DIScope .DILocalScope . DISubprogram $
                    Subprogram
                      { scope = Nothing
                      , name = "main"
                      , linkageName = ""
                      , file = Nothing
                      , line = 0
                      , type' = Nothing
                      , localToUnit = False
                      , definition = True
                      , scopeLine = 0
                      , containingType = Nothing
                      , virtuality = NoVirtuality
                      , virtualityIndex = 0
                      , thisAdjustment = 0
                      , flags = []
                      , optimized = False
                      , unit = Just (MDRef (MetadataNodeID 2))
                      , O.templateParams = []
                      , declaration = Nothing
                      , retainedNodes = []
                      , thrownTypes = []
                      }
                  , MetadataNodeDefinition (MetadataNodeID 1)
                    (MDTuple [ Just (MDValue (ConstantOperand (C.Int 32 2)))
                             , Just (MDString "Debug Info Version")
                             , Just (MDValue (ConstantOperand (C.Int 32 3)))
                             ])
                  , MetadataNodeDefinition (MetadataNodeID 2) $
                    DINode . DIScope . DICompileUnit $
                    CompileUnit
                      { language = 12
                      , file = MDRef (MetadataNodeID 3)
                      , producer = "clang version 6.0.0 (tags/RELEASE_600/final)"
                      , optimized = True
                      , flags = ""
                      , runtimeVersion = 0
                      , splitDebugFileName = ""
                      , emissionKind = FullDebug
                      , enums = []
                      , retainedTypes = []
                      , globals = []
                      , imports = []
                      , macros = []
                      , dWOId = 0
                      , splitDebugInlining = True
                      , debugInfoForProfiling = False
                      , nameTableKind = NameTableKindDefault
                      , debugBaseAddress = False
                      }
                  , MetadataNodeDefinition (MetadataNodeID 3) $
                    DINode . DIScope . DIFile $
                    O.File "main.c" "/" Nothing
                  ]
          s =
            "; ModuleID = '<string>'\n\
            \source_filename = \"<string>\"\n\
            \\n\
            \define void @main() !dbg !3 {\n\
            \  ret void\n\
            \}\n\
            \\n\
            \!llvm.module.flags = !{!0}\n\
            \!llvm.dbg.cu = !{!1}\n\
            \\n\
            \!0 = !{i32 2, !\"Debug Info Version\", i32 3}\n\
            \!1 = distinct !DICompileUnit(language: DW_LANG_C99, file: !2, producer: \"clang version 6.0.0 (tags/RELEASE_600/final)\", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug)\n\
            \!2 = !DIFile(filename: \"main.c\", directory: \"/\")\n\
            \!3 = distinct !DISubprogram(name: \"main\", scope: null, spFlags: DISPFlagDefinition, unit: !1)\n"
      strCheck ast s,
   testCase "metadata on global variables" $ do
      let ast = Module "<string>" "<string>" Nothing Nothing
                  [ GlobalDefinition
                      globalVariableDefaults
                        { G.name = "g"
                        , G.type' = A.T.i32
                        , G.linkage = L.Common
                        , G.alignment = 4
                        , G.initializer = Just (C.Int 32 0)
                        , G.metadata = [("dbg", MDRef (MetadataNodeID 0))]
                        }
                  , NamedMetadataDefinition "llvm.module.flags" [MetadataNodeID 1]
                  , NamedMetadataDefinition "llvm.dbg.cu" [MetadataNodeID 2]
                  , MetadataNodeDefinition (MetadataNodeID 0) $
                    DIGlobalVariableExpression $ GlobalVariableExpression (MDRef (MetadataNodeID 3)) (MDRef (MetadataNodeID 4))
                  , MetadataNodeDefinition (MetadataNodeID 1)
                    (MDTuple [ Just (MDValue (ConstantOperand (C.Int 32 2)))
                             , Just (MDString "Debug Info Version")
                             , Just (MDValue (ConstantOperand (C.Int 32 3)))
                             ])
                  , MetadataNodeDefinition (MetadataNodeID 2) $
                    DINode . DIScope . DICompileUnit $
                    CompileUnit
                      { language = 12
                      , file = MDRef (MetadataNodeID 5)
                      , producer = "clang version 6.0.0 (tags/RELEASE_600/final)"
                      , optimized = True
                      , flags = ""
                      , runtimeVersion = 0
                      , splitDebugFileName = ""
                      , emissionKind = FullDebug
                      , enums = []
                      , retainedTypes = []
                      , globals = []
                      , imports = []
                      , macros = []
                      , dWOId = 0
                      , splitDebugInlining = True
                      , debugInfoForProfiling = False
                      , nameTableKind = NameTableKindDefault
                      , debugBaseAddress = False
                      }
                  , MetadataNodeDefinition (MetadataNodeID 3) $
                    DINode . DIVariable . DIGlobalVariable $
                    GlobalVariable
                      { name = "g"
                      , scope = Nothing
                      , file = Nothing
                      , line = 0
                      , type' = Just (MDRef (MetadataNodeID 6))
                      , linkageName = ""
                      , local = False
                      , definition = True
                      , staticDataMemberDeclaration = Nothing
                      , templateParams = []
                      , alignInBits = 0
                      }
                  , MetadataNodeDefinition (MetadataNodeID 4) (DIExpression (Expression []))
                  , MetadataNodeDefinition (MetadataNodeID 5) $
                    DINode . DIScope . DIFile $
                    O.File "main.c" "/" Nothing
                  , MetadataNodeDefinition (MetadataNodeID 6) $
                    DINode . DIScope . DIType . DIBasicType $
                    BasicType "" 0 0 Nothing BaseType []
                  ]
          s =
            "; ModuleID = '<string>'\n\
            \source_filename = \"<string>\"\n\
            \\n\
            \@g = common global i32 0, align 4, !dbg !0\n\
            \\n\
            \!llvm.module.flags = !{!3}\n\
            \!llvm.dbg.cu = !{!4}\n\
            \\n\
            \!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())\n\
            \!1 = !DIGlobalVariable(name: \"g\", scope: null, type: !2, isLocal: false, isDefinition: true)\n\
            \!2 = !DIBasicType()\n\
            \!3 = !{i32 2, !\"Debug Info Version\", i32 3}\n\
            \!4 = distinct !DICompileUnit(language: DW_LANG_C99, file: !5, producer: \"clang version 6.0.0 (tags/RELEASE_600/final)\", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug)\n\
            \!5 = !DIFile(filename: \"main.c\", directory: \"/\")\n"
      strCheck ast s
  ]
