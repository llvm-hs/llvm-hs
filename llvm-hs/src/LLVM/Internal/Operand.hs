{-# LANGUAGE
  DuplicateRecordFields,
  MultiParamTypeClasses,
  NamedFieldPuns,
  OverloadedStrings,
  PatternSynonyms,
  QuasiQuotes,
  RecordWildCards,
  ScopedTypeVariables,
  TemplateHaskell
  #-}
module LLVM.Internal.Operand where

import LLVM.Prelude

import LLVM.Exception

import Control.Monad.Catch
import Control.Monad.State
import Control.Monad.AnyCont
import Data.Bits
import Data.Functor.Identity
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

import Foreign.Ptr

import qualified LLVM.Internal.FFI.Constant as FFI
import qualified LLVM.Internal.FFI.InlineAssembly as FFI
import qualified LLVM.Internal.FFI.LLVMCTypes as FFI
import qualified LLVM.Internal.FFI.Metadata as FFI
import qualified LLVM.Internal.FFI.PtrHierarchy as FFI
import qualified LLVM.Internal.FFI.Value as FFI

import LLVM.Internal.Coding
import LLVM.Internal.Constant ()
import LLVM.Internal.Context
import LLVM.Internal.DecodeAST
import LLVM.Internal.EncodeAST
import LLVM.Internal.InlineAssembly ()
import LLVM.Internal.Metadata (getByteStringFromFFI)

import qualified LLVM.AST as A hiding (GlobalVariable, Module, PointerType, type')
import qualified LLVM.AST.Operand as A

import LLVM.Internal.FFI.LLVMCTypes (mdSubclassIdP)

instance EncodeM EncodeAST ShortByteString (Ptr FFI.MDString) where
  encodeM s = do
    s' <- encodeM s
    Context c <- gets encodeStateContext
    liftIO (FFI.getMDString c s')

instance Applicative m => EncodeM m [A.DIFlag] FFI.DIFlags where
  encodeM fs = pure (FFI.DIFlags (foldl' (.|.) 0 (map encodeFlag fs)))
    where
      encodeFlag f =
        case f of
          A.Accessibility A.Private -> 1
          A.Accessibility A.Protected -> 2
          A.Accessibility A.Public -> 3
          A.FwdDecl -> 1 `shiftL` 2
          A.AppleBlock -> 1 `shiftL` 3
          A.BlockByrefStruct -> 1 `shiftL` 4
          A.VirtualFlag -> 1 `shiftL` 5
          A.Artificial -> 1 `shiftL` 6
          A.Explicit -> 1 `shiftL` 7
          A.Prototyped -> 1 `shiftL` 8
          A.ObjcClassComplete -> 1 `shiftL` 9
          A.ObjectPointer -> 1 `shiftL` 10
          A.Vector -> 1 `shiftL` 11
          A.StaticMember -> 1 `shiftL` 12
          A.LValueReference -> 1 `shiftL` 13
          A.RValueReference -> 1 `shiftL` 14
          A.InheritanceFlag A.SingleInheritance -> 1 `shiftL` 16
          A.InheritanceFlag A.MultipleInheritance -> 2 `shiftL` 16
          A.InheritanceFlag A.VirtualInheritance -> 3 `shiftL` 16
          A.IntroducedVirtual -> 1 `shiftL` 18
          A.BitField -> 1 `shiftL` 19
          A.NoReturn -> 1 `shiftL` 20
          A.ArgumentNotModified -> 1 `shiftL` 21
          A.TypePassByValue -> 1 `shiftL` 22
          A.TypePassByReference -> 1 `shiftL` 23
          A.EnumClass -> 1 `shiftL` 24
          A.Thunk -> 1 `shiftL` 25
          A.NonTrivial -> 1 `shiftL` 26
          A.BigEndian -> 1 `shiftL` 27
          A.LittleEndian -> 1 `shiftL` 28
          A.AllCallsDescribed -> 1 `shiftL` 29

instance Applicative m => DecodeM m [A.DIFlag] FFI.DIFlags where
  decodeM (FFI.DIFlags f) =
    pure $
    accessibility ++
    inheritance ++
    mapMaybe flagSet flags
    where
      flagSet :: A.DIFlag -> Maybe A.DIFlag
      flagSet f'
        | f'' .&. f /= 0 = Just f'
        | otherwise = Nothing
        where Identity (FFI.DIFlags f'') = encodeM [f']
      accessibility
        | testBit f 0 && testBit f 1 = [A.Accessibility A.Public]
        | testBit f 0 = [A.Accessibility A.Private]
        | testBit f 1 = [A.Accessibility A.Protected]
        | otherwise = []
      inheritance
        | testBit f 16 && testBit f 17 = [A.InheritanceFlag A.VirtualInheritance]
        | testBit f 16 = [A.InheritanceFlag A.SingleInheritance]
        | testBit f 17 = [A.InheritanceFlag A.MultipleInheritance]
        | otherwise = []
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

instance DecodeM DecodeAST A.Operand (Ptr FFI.Value) where
  decodeM v = do
    c <- liftIO $ FFI.isAConstant v
    if c /= nullPtr
     then
      return A.ConstantOperand `ap` decodeM c
     else
      do m <- liftIO $ FFI.isAMetadataOperand v
         if m /= nullPtr
            then A.MetadataOperand <$> decodeM m
            else A.LocalReference
                   <$> (decodeM =<< liftIO (FFI.typeOf v))
                   <*> getLocalName v

instance DecodeM DecodeAST A.Metadata (Ptr FFI.Metadata) where
  decodeM md = do
    s <- liftIO $ FFI.isAMDString md
    if s /= nullPtr
      then A.MDString <$> decodeM s
      else do
        n <- liftIO $ FFI.isAMDNode md
        if n /= nullPtr
          then A.MDNode <$> decodeM n
          else do v <- liftIO $ FFI.isAMDValue md
                  if v /= nullPtr
                    then A.MDValue <$> decodeM v
                    else throwM (DecodeException "Metadata was not one of [MDString, MDValue, MDNode]")

instance DecodeM DecodeAST (Either String A.DINode) (Ptr FFI.DINode) where
  decodeM diN = do
    sId <- liftIO $ FFI.getMetadataClassId (FFI.upCast diN)
    case sId of
      [mdSubclassIdP|DIEnumerator|] ->
        liftM Right $ A.DIEnumerator <$> decodeM (castPtr diN :: Ptr FFI.DIEnumerator)
      [mdSubclassIdP|DIImportedEntity|]   -> liftM Right $ A.DIImportedEntity <$> decodeM (castPtr diN :: Ptr FFI.DIImportedEntity)
      [mdSubclassIdP|DIObjCProperty|]     -> liftM Right $ A.DIObjCProperty <$> decodeM (castPtr diN :: Ptr FFI.DIObjCProperty)
      [mdSubclassIdP|DISubrange|]         -> liftM Right $ A.DISubrange <$> decodeM (castPtr diN :: Ptr FFI.DISubrange)
      [mdSubclassIdP|DIBasicType|]        -> liftM Right $ A.DIScope <$> decodeM (castPtr diN :: Ptr FFI.DIScope)
      [mdSubclassIdP|DICompositeType|]    -> liftM Right $ A.DIScope <$> decodeM (castPtr diN :: Ptr FFI.DIScope)
      [mdSubclassIdP|DIDerivedType|]      -> liftM Right $ A.DIScope <$> decodeM (castPtr diN :: Ptr FFI.DIScope)
      [mdSubclassIdP|DISubroutineType|]   -> liftM Right $ A.DIScope <$> decodeM (castPtr diN :: Ptr FFI.DIScope)
      [mdSubclassIdP|DILexicalBlock|]     -> liftM Right $ A.DIScope <$> decodeM (castPtr diN :: Ptr FFI.DIScope)
      [mdSubclassIdP|DILexicalBlockFile|] -> liftM Right $ A.DIScope <$> decodeM (castPtr diN :: Ptr FFI.DIScope)
      [mdSubclassIdP|DIFile|]             -> liftM Right $ A.DIScope <$> decodeM (castPtr diN :: Ptr FFI.DIScope)
      [mdSubclassIdP|DINamespace|]        -> liftM Right $ A.DIScope <$> decodeM (castPtr diN :: Ptr FFI.DIScope)
      [mdSubclassIdP|DISubprogram|]       -> liftM Right $ A.DIScope <$> decodeM (castPtr diN :: Ptr FFI.DIScope)
      [mdSubclassIdP|DICompileUnit|]      -> liftM Right $ A.DIScope <$> decodeM (castPtr diN :: Ptr FFI.DIScope)
      [mdSubclassIdP|DIModule|]           -> liftM Right $ A.DIScope <$> decodeM (castPtr diN :: Ptr FFI.DIScope)

      [mdSubclassIdP|DIGlobalVariable|]   -> liftM Right $ A.DIVariable <$> decodeM (castPtr diN :: Ptr FFI.DIVariable)
      [mdSubclassIdP|DILocalVariable|]    -> liftM Right $ A.DIVariable <$> decodeM (castPtr diN :: Ptr FFI.DIVariable)

      [mdSubclassIdP|DITemplateTypeParameter|]  -> liftM Right $ A.DITemplateParameter <$> decodeM (castPtr diN :: Ptr FFI.DITemplateParameter)
      [mdSubclassIdP|DITemplateValueParameter|] -> liftM Right $ A.DITemplateParameter <$> decodeM (castPtr diN :: Ptr FFI.DITemplateParameter)

      [mdSubclassIdP|MDString|] -> liftM Left $ pure ("Lifting to Haskell not implemented for toplevel DINode kind: " <> show sId <> " (MDString)")
      [mdSubclassIdP|ConstantAsMetadata|] -> liftM Left $ pure ("Lifting to Haskell not implemented for toplevel DINode kind: " <> show sId <> " (ConstantAsMetadata)")
      [mdSubclassIdP|LocalAsMetadata|] -> liftM Left $ pure ("Lifting to Haskell not implemented for toplevel DINode kind: " <> show sId <> " (LocalAsMetadata)")
      [mdSubclassIdP|DistinctMDOperandPlaceholder|] -> liftM Left $ pure ("Lifting to Haskell not implemented for toplevel DINode kind: " <> show sId <> " (DistinctMDOperandPlaceholder)")
      [mdSubclassIdP|MDTuple|] -> liftM Left $ pure ("Lifting to Haskell not implemented for toplevel DINode kind: " <> show sId <> " (MDTuple)")
      [mdSubclassIdP|DILocation|] -> liftM Left $ pure ("Lifting to Haskell not implemented for toplevel DINode kind: " <> show sId <> " (DILocation)")
      [mdSubclassIdP|DIExpression|] -> liftM Left $ pure ("Lifting to Haskell not implemented for toplevel DINode kind: " <> show sId <> " (DIExpression)")
      [mdSubclassIdP|DIGlobalVariableExpression|] -> liftM Left $ pure ("Lifting to Haskell not implemented for toplevel DINode kind: " <> show sId <> " (DIGlobalVariableExpression)")
      [mdSubclassIdP|GenericDINode|] -> liftM Left $ pure ("Lifting to Haskell not implemented for toplevel DINode kind: " <> show sId <> " (GenericDINode)")
      [mdSubclassIdP|DIMacro|] -> liftM Left $ pure ("Lifting to Haskell not implemented for toplevel DINode kind: " <> show sId <> " (DIMacro)")
      [mdSubclassIdP|DIMacroFile|] -> liftM Left $ pure ("Lifting to Haskell not implemented for toplevel DINode kind: " <> show sId <> " (DIMacroFile)")

      _ -> throwM (DecodeException ("Unknown subclass id for DINode: " <> show sId))

instance EncodeM EncodeAST A.DISubrange (Ptr FFI.DISubrange) where
  encodeM (A.Subrange {..}) = do
    Context c <- gets encodeStateContext
    case count of
      A.DICountConstant i -> liftIO (FFI.getDISubrangeConstantCount c i lowerBound)
      A.DICountVariable v -> do
        v' <- encodeM v
        liftIO (FFI.getDISubrangeVariableCount c v' lowerBound)

instance DecodeM DecodeAST A.DISubrange (Ptr FFI.DISubrange) where
  decodeM r = do
    lowerBound <- liftIO (FFI.getDISubrangeLowerBound r)
    hasConstantCount <- decodeM =<< liftIO (FFI.getDISubrangeHasConstantCount r)
    if hasConstantCount
      then do
        count <- liftIO (FFI.getDISubrangeCountConstant r)
        pure (A.Subrange (A.DICountConstant count) lowerBound)
      else do
        count <- decodeM =<< liftIO (FFI.getDISubrangeCountVariable r)
        pure (A.Subrange (A.DICountVariable count) lowerBound)

instance EncodeM EncodeAST A.DIEnumerator (Ptr FFI.DIEnumerator) where
  encodeM (A.Enumerator {..}) = do
    name <- encodeM name
    isUnsigned <- encodeM isUnsigned
    Context c <- gets encodeStateContext
    liftIO (FFI.getDIEnumerator c value isUnsigned name)

instance DecodeM DecodeAST A.DIEnumerator (Ptr FFI.DIEnumerator) where
  decodeM e = do
    value <- liftIO (FFI.getDIEnumeratorValue e)
    isUnsigned <- decodeM =<< liftIO (FFI.getDIEnumeratorIsUnsigned e)
    name <- decodeM =<< liftIO (FFI.getDIEnumeratorName e)
    pure (A.Enumerator value isUnsigned name)

instance EncodeM EncodeAST A.DINode (Ptr FFI.DINode) where
  encodeM (A.DISubrange r) = do
    ptr <- encodeM r
    pure (FFI.upCast (ptr :: Ptr FFI.DISubrange))
  encodeM (A.DIEnumerator e) = do
    ptr <- encodeM e
    pure (FFI.upCast (ptr :: Ptr FFI.DIEnumerator))
  encodeM (A.DIScope s) = do
    ptr <- encodeM s
    pure (FFI.upCast (ptr :: Ptr FFI.DIScope))
  encodeM (A.DIVariable v) = do
    ptr <- encodeM v
    pure (FFI.upCast (ptr :: Ptr FFI.DIVariable))
  encodeM (A.DITemplateParameter p) = do
    ptr <- encodeM p
    pure (FFI.upCast (ptr :: Ptr FFI.DITemplateParameter))
  encodeM (A.DIImportedEntity e) = do
    ptr <- encodeM e
    pure (FFI.upCast (ptr :: Ptr FFI.DIImportedEntity))
  encodeM (A.DIObjCProperty o) = FFI.upCast <$> (encodeM o :: EncodeAST (Ptr FFI.DIObjCProperty))

instance DecodeM DecodeAST A.DIScope (Ptr FFI.DIScope) where
  decodeM p = do
    sId <- liftIO $ FFI.getMetadataClassId (FFI.upCast p)
    case sId of
      [mdSubclassIdP|DINamespace|] -> A.DINamespace <$> decodeM (castPtr p :: Ptr FFI.DINamespace)
      [mdSubclassIdP|DIFile|] -> A.DIFile <$> decodeM (castPtr p :: Ptr FFI.DIFile)
      [mdSubclassIdP|DILexicalBlock|]     -> A.DILocalScope <$> decodeM (castPtr p :: Ptr FFI.DILocalScope)
      [mdSubclassIdP|DILexicalBlockFile|] -> A.DILocalScope <$> decodeM (castPtr p :: Ptr FFI.DILocalScope)
      [mdSubclassIdP|DISubprogram|]       -> A.DILocalScope <$> decodeM (castPtr p :: Ptr FFI.DILocalScope)

      [mdSubclassIdP|DIBasicType|]      -> A.DIType <$> decodeM (castPtr p :: Ptr FFI.DIType)
      [mdSubclassIdP|DICompositeType|]  -> A.DIType <$> decodeM (castPtr p :: Ptr FFI.DIType)
      [mdSubclassIdP|DIDerivedType|]    -> A.DIType <$> decodeM (castPtr p :: Ptr FFI.DIType)
      [mdSubclassIdP|DISubroutineType|] -> A.DIType <$> decodeM (castPtr p :: Ptr FFI.DIType)

      [mdSubclassIdP|DICompileUnit|] -> A.DICompileUnit <$> decodeM (castPtr p :: Ptr FFI.DICompileUnit)
      [mdSubclassIdP|DIModule|]      -> A.DIModule <$> decodeM (castPtr p :: Ptr FFI.DIModule)

      _ -> throwM (DecodeException ("Unknown subclass id for DIScope: " <> show sId))

instance DecodeM DecodeAST A.DINamespace (Ptr FFI.DINamespace) where
  decodeM p = do
    scope <- decodeM =<< liftIO (FFI.getScopeScope (FFI.upCast p))
    name  <- getByteStringFromFFI FFI.getScopeName (FFI.upCast p)
    exported <- decodeM =<< liftIO (FFI.getNamespaceExportedSymbols (castPtr p))
    return (A.Namespace name scope exported)

instance EncodeM EncodeAST A.DINamespace (Ptr FFI.DINamespace) where
  encodeM A.Namespace {..} = do
    name <- encodeM name
    scope <- encodeM scope
    exportSyms <- encodeM exportSymbols
    Context c <- gets encodeStateContext
    liftIO (FFI.getDINamespace c scope name exportSyms)

instance DecodeM DecodeAST A.DIModule (Ptr FFI.DIModule) where
  decodeM p = do
    scope <- decodeM =<< liftIO (FFI.getScopeScope (FFI.upCast p))
    name <- getByteStringFromFFI FFI.getScopeName (FFI.upCast p)
    let m = castPtr p :: Ptr FFI.DIModule
    configurationMacros <- decodeM =<< liftIO (FFI.getDIModuleConfigurationMacros m)
    includePath <- decodeM =<< liftIO (FFI.getDIModuleIncludePath m)
    isysRoot <- decodeM =<< liftIO (FFI.getDIModuleISysRoot m)
    pure A.Module
      { A.scope = scope
      , A.name = name
      , A.configurationMacros = configurationMacros
      , A.includePath = includePath
      , A.isysRoot = isysRoot
      }

instance EncodeM EncodeAST A.DIModule (Ptr FFI.DIModule) where
  encodeM A.Module {..} = do
    scope <- encodeM scope
    name <- encodeM name
    configurationMacros <- encodeM configurationMacros
    includePath <- encodeM includePath
    isysRoot <- encodeM isysRoot
    Context c <- gets encodeStateContext
    liftIO (FFI.getDIModule c scope name configurationMacros includePath isysRoot)

genCodingInstance [t|A.DebugEmissionKind|] ''FFI.DebugEmissionKind
  [ (FFI.NoDebug, A.NoDebug)
  , (FFI.FullDebug, A.FullDebug)
  , (FFI.LineTablesOnly, A.LineTablesOnly)
  ]

genCodingInstance [t|A.DebugNameTableKind|] ''FFI.DebugNameTableKind
  [ (FFI.NameTableKindDefault, A.NameTableKindDefault)
  , (FFI.NameTableKindGNU, A.NameTableKindGNU)
  , (FFI.NameTableKindNone, A.NameTableKindNone)
  ]

instance DecodeM DecodeAST A.DICompileUnit (Ptr FFI.DICompileUnit) where
  decodeM p = do
    language <- decodeM =<< liftIO (FFI.getDICompileUnitLanguage p)
    file <- decodeM =<< liftIO (FFI.getScopeFile (FFI.upCast p))
    producer <- decodeM =<< liftIO (FFI.getDICompileUnitProducer p)
    optimized <- decodeM =<< liftIO (FFI.getDICompileUnitOptimized p)
    flags <- decodeM =<< liftIO (FFI.getDICompileUnitFlags p)
    runtimeVersion <- decodeM =<< liftIO (FFI.getDICompileUnitRuntimeVersion p)
    splitDebugFilename <- decodeM =<< liftIO (FFI.getDICompileUnitSplitDebugFilename p)
    emissionKind <- decodeM =<< liftIO (FFI.getDICompileUnitEmissionKind p)
    dwoid <- decodeM =<< liftIO (FFI.getDICompileUnitDWOId p)
    splitDebugInlining <- decodeM =<< liftIO (FFI.getDICompileUnitSplitDebugInlining p)
    debugInfoForProfiling <- decodeM =<< liftIO (FFI.getDICompileUnitDebugInfoForProfiling p)
    enums <- decodeM =<< liftIO (FFI.getDICompileUnitEnumTypes p)
    retainedTypes' <- decodeM =<< liftIO (FFI.getDICompileUnitRetainedTypes p)
    let toRetainedType :: A.MDRef A.DIScope -> DecodeAST (A.MDRef (Either A.DIType A.DISubprogram))
        toRetainedType (A.MDRef i) = pure (A.MDRef i)
        toRetainedType (A.MDInline (A.DIType ty)) = pure (A.MDInline (Left ty))
        toRetainedType (A.MDInline (A.DILocalScope (A.DISubprogram p))) = pure (A.MDInline (Right p))
        toRetainedType (A.MDInline e) = throwM (DecodeException ("Retained type must be DISubprogram or DIType but got " <> show e))
    retainedTypes <- traverse toRetainedType retainedTypes'
    globals <- decodeM =<< liftIO (FFI.getDICompileUnitGlobalVariables p)
    entities <- decodeM =<< liftIO (FFI.getDICompileUnitImportedEntities p)
    macros <- decodeM =<< liftIO (FFI.getDICompileUnitMacros p)
    nameTableKind <- decodeM =<< liftIO (FFI.getDICompileUnitNameTableKind p)
    debugBaseAddress <- decodeM =<< liftIO (FFI.getDICompileUnitRangesBaseAddress p)
    pure A.CompileUnit
      { A.language = language
      , A.file = file
      , A.producer = producer
      , A.optimized = optimized
      , A.flags = flags
      , A.runtimeVersion = runtimeVersion
      , A.splitDebugFileName = splitDebugFilename
      , A.emissionKind = emissionKind
      , A.enums = enums
      , A.retainedTypes = retainedTypes
      , A.globals = globals
      , A.imports = entities
      , A.macros = macros
      , A.dWOId = dwoid
      , A.splitDebugInlining = splitDebugInlining
      , A.debugInfoForProfiling = debugInfoForProfiling
      , A.nameTableKind = nameTableKind
      , A.debugBaseAddress = debugBaseAddress
      }

instance EncodeM EncodeAST A.DICompileUnit (Ptr FFI.DICompileUnit) where
  encodeM (A.CompileUnit {..}) = do
    language <- encodeM language
    file <- encodeM file
    producer <- encodeM producer
    optimized <- encodeM optimized
    flags <- encodeM flags
    runtimeVersion <- encodeM runtimeVersion
    debugFileName <- encodeM splitDebugFileName
    emissionKind <- encodeM emissionKind
    enums <- encodeM enums
    retainedTypes <- encodeM (map (fmap (either A.DIType (A.DILocalScope . A.DISubprogram))) retainedTypes)
    globals <- encodeM globals
    imports <- encodeM imports
    macros <- encodeM macros
    dwoid <- encodeM dWOId
    splitDebugInlining <- encodeM splitDebugInlining
    debugInfoForProfiling <- encodeM debugInfoForProfiling
    nameTableKind <- encodeM nameTableKind
    debugBaseAddress <- encodeM debugBaseAddress
    Context c <- gets encodeStateContext
    liftIO $ FFI.getDICompileUnit
      c
      language file producer optimized flags
      runtimeVersion debugFileName emissionKind enums retainedTypes
      globals imports macros dwoid splitDebugInlining
      debugInfoForProfiling nameTableKind debugBaseAddress

instance EncodeM EncodeAST A.DIScope (Ptr FFI.DIScope) where
  encodeM (A.DIFile f) = FFI.upCast <$> (encodeM f :: EncodeAST (Ptr FFI.DIFile))
  encodeM (A.DICompileUnit cu) = FFI.upCast <$> (encodeM cu :: EncodeAST (Ptr FFI.DICompileUnit))
  encodeM (A.DIType t) = FFI.upCast <$> (encodeM t :: EncodeAST (Ptr FFI.DIType))
  encodeM (A.DILocalScope t) = FFI.upCast <$> (encodeM t :: EncodeAST (Ptr FFI.DILocalScope))
  encodeM (A.DINamespace ns) = FFI.upCast <$> (encodeM ns :: EncodeAST (Ptr FFI.DINamespace))
  encodeM (A.DIModule m) = FFI.upCast <$> (encodeM m :: EncodeAST (Ptr FFI.DIModule))

instance DecodeM DecodeAST A.DIFile (Ptr FFI.DIFile) where
  decodeM diF = do
    fname <- decodeM =<< liftIO (FFI.getFileFilename diF)
    dir   <- decodeM =<< liftIO (FFI.getFileDirectory diF)
    mayChecksum <- decodeM =<< liftIO (FFI.getFileChecksumValue diF)
    case mayChecksum of
      Nothing -> pure (A.File fname dir Nothing)
      Just checksum -> do
        csk <- decodeM =<< liftIO (FFI.getFileChecksumKind diF)
        pure (A.File fname dir (Just (A.ChecksumInfo csk checksum)))

instance EncodeM EncodeAST A.DIFile (Ptr FFI.DIFile) where
  encodeM (A.File {..}) = do
    filename <- encodeM filename
    directory <- encodeM directory
    Context c <- gets encodeStateContext
    case checksum of
      Nothing -> liftIO (FFI.getDIFile c filename directory (FFI.ChecksumKind 0) nullPtr)
      Just (A.ChecksumInfo {..}) -> do
        checksumValue <- encodeM checksumValue
        checksumKind <- encodeM checksumKind
        liftIO (FFI.getDIFile c filename directory checksumKind checksumValue)

instance EncodeM EncodeAST (Maybe A.Encoding) FFI.Encoding where
  encodeM Nothing = pure (FFI.Encoding 0)
  encodeM (Just e) = encodeM e

instance DecodeM DecodeAST (Maybe A.Encoding) FFI.Encoding where
  decodeM (FFI.Encoding 0) = pure Nothing
  decodeM e = Just <$> decodeM e

genCodingInstance [t|A.Encoding|] ''FFI.Encoding
  [ (FFI.DwAtE_address, A.AddressEncoding)
  , (FFI.DwAtE_boolean, A.BooleanEncoding)
  , (FFI.DwAtE_float, A.FloatEncoding)
  , (FFI.DwAtE_signed, A.SignedEncoding)
  , (FFI.DwAtE_signed_char, A.SignedCharEncoding)
  , (FFI.DwAtE_unsigned, A.UnsignedEncoding)
  , (FFI.DwAtE_unsigned_char, A.UnsignedCharEncoding)
  , (FFI.DwAtE_UTF, A.UTFEncoding)
  ]

genCodingInstance [t|A.ChecksumKind|] ''FFI.ChecksumKind
  [ (FFI.ChecksumKind 1, A.MD5)
  , (FFI.ChecksumKind 2, A.SHA1)
  ]

genCodingInstance [t|A.BasicTypeTag|] ''FFI.DwTag
  [ (FFI.DwTag_base_type, A.BaseType)
  , (FFI.DwTag_unspecified_type, A.UnspecifiedType)
  ]

instance EncodeM EncodeAST A.DIType (Ptr FFI.DIType) where
  encodeM (A.DIBasicType t) = FFI.upCast <$> (encodeM t :: EncodeAST (Ptr FFI.DIBasicType))
  encodeM (A.DIDerivedType t) = FFI.upCast <$> (encodeM t :: EncodeAST (Ptr FFI.DIDerivedType))
  encodeM (A.DISubroutineType t) = FFI.upCast <$> (encodeM t :: EncodeAST (Ptr FFI.DISubroutineType))
  encodeM (A.DICompositeType t) = FFI.upCast <$> (encodeM t :: EncodeAST (Ptr FFI.DICompositeType))

instance DecodeM DecodeAST A.DICompositeType (Ptr FFI.DICompositeType) where
  decodeM diTy = do
    tag <- liftIO (FFI.getTag (FFI.upCast diTy))
    size <- liftIO (FFI.getTypeSizeInBits (FFI.upCast diTy))
    align <- liftIO (FFI.getTypeAlignInBits (FFI.upCast diTy))
    elements <- liftIO (FFI.getElements diTy)
    scope <- decodeM =<< liftIO (FFI.getScopeScope (FFI.upCast diTy))
    name <- decodeM =<< liftIO (FFI.getTypeName (FFI.upCast diTy))
    file <- decodeM =<< liftIO (FFI.getScopeFile (FFI.upCast diTy))
    line <- liftIO (FFI.getTypeLine (FFI.upCast diTy))
    flags <- decodeM =<< liftIO (FFI.getTypeFlags (FFI.upCast diTy))
    runtimeLang <- liftIO (FFI.getRuntimeLang diTy)
    vtableHolder <- decodeM =<< liftIO (FFI.getVTableHolder diTy)
    type' <- decodeM =<< liftIO (FFI.getCompositeBaseType diTy)
    identifier <- decodeM =<< liftIO (FFI.getIdentifier diTy)
    case tag of
      FFI.DwTag_array_type -> do
        subscripts <- decodeM (FFI.TupleArray elements :: FFI.TupleArray FFI.DISubrange)
        pure (A.DIArrayType subscripts type' size align flags)
      FFI.DwTag_enumeration_type -> do
        values <- decodeM (FFI.TupleArray elements :: FFI.TupleArray FFI.DIEnumerator)
        pure (A.DIEnumerationType scope name file line values type' identifier size align)
      FFI.DwTag_structure_type -> do
        elements <- decodeM (FFI.TupleArray elements :: FFI.TupleArray FFI.DIScope)
        elements <- traverse fromCompElement elements
        pure (A.DIStructureType scope name file line flags type' elements runtimeLang vtableHolder identifier size align)
      FFI.DwTag_class_type -> do
        elements <- decodeM (FFI.TupleArray elements :: FFI.TupleArray FFI.DIScope)
        elements <- traverse fromCompElement elements
        vtableHolder <- decodeM =<< liftIO (FFI.getVTableHolder diTy)
        params <- decodeM =<< liftIO (FFI.getTemplateParams diTy)
        pure (A.DIClassType scope name file line flags type' elements vtableHolder params identifier size align)
      FFI.DwTag_union_type -> do
        elements <- decodeM (FFI.TupleArray elements :: FFI.TupleArray FFI.DIScope)
        elements <- traverse fromCompElement elements
        pure (A.DIUnionType scope name file line flags elements runtimeLang identifier size align)
      _ -> throwM (DecodeException ("Unknown tag of DICompositeType: " <> show tag))

fromCompElement :: A.MDRef A.DIScope -> DecodeAST (A.MDRef (Either A.DIDerivedType A.DISubprogram))
fromCompElement (A.MDRef r) = pure (A.MDRef r)
fromCompElement (A.MDInline (A.DIType (A.DIDerivedType ty))) = pure (A.MDInline (Left ty))
fromCompElement (A.MDInline (A.DILocalScope (A.DISubprogram p))) = pure (A.MDInline (Right p))
fromCompElement (A.MDInline e) = throwM (DecodeException ("Elements of DIClassType, DIStructureType and DIUnionType must be DIDerivedType or DISubprogram but got " <> show e))

toCompElement :: A.MDRef (Either A.DIDerivedType A.DISubprogram) -> A.MDRef A.DIScope
toCompElement = fmap (either (A.DIType . A.DIDerivedType) (A.DILocalScope . A.DISubprogram))

instance EncodeM EncodeAST A.DICompositeType (Ptr FFI.DICompositeType) where
  encodeM A.DIArrayType {..} = do
    subscripts <- encodeM subscripts
    elementTy <- encodeM elementTy
    flags <- encodeM flags
    Context c <- gets encodeStateContext
    liftIO (FFI.getDIArrayType c subscripts elementTy sizeInBits alignInBits flags)
  encodeM A.DIEnumerationType {..} = do
    scope <- encodeM scope
    name <- encodeM name
    file <- encodeM file
    elements <- encodeM values
    underlyingType <- encodeM baseType
    identifier <- encodeM identifier
    Context c <- gets encodeStateContext
    liftIO (FFI.getDIEnumerationType c scope name file line sizeInBits alignInBits elements underlyingType identifier)
  encodeM A.DIStructureType {..} = do
    scope <- encodeM scope
    name <- encodeM name
    file <- encodeM file
    flags <- encodeM flags
    derivedFrom <- encodeM derivedFrom
    elements <- encodeM (map toCompElement elements)
    vtableHolder <- encodeM vtableHolder
    identifier <- encodeM identifier
    Context c <- gets encodeStateContext
    liftIO (FFI.getDIStructType c scope name file line sizeInBits alignInBits flags derivedFrom elements runtimeLang vtableHolder identifier)
  encodeM A.DIUnionType {..} = do
    scope <- encodeM scope
    name <- encodeM name
    file <- encodeM file
    flags <- encodeM flags
    elements <- encodeM (map toCompElement elements)
    identifier <- encodeM identifier
    Context c <- gets encodeStateContext
    liftIO (FFI.getDIUnionType c scope name file line sizeInBits alignInBits flags elements runtimeLang identifier)
  encodeM A.DIClassType {..} = do
    scope <- encodeM scope
    name <- encodeM name
    file <- encodeM file
    flags <- encodeM flags
    derivedFrom <- encodeM derivedFrom
    elements <- encodeM (map toCompElement elements)
    vtableHolder <- encodeM vtableHolder
    params <- encodeM templateParams
    identifier <- encodeM identifier
    Context c <- gets encodeStateContext
    liftIO (FFI.getDIClassType c scope name file line sizeInBits alignInBits flags derivedFrom elements vtableHolder params identifier)

instance DecodeM DecodeAST A.DIType (Ptr FFI.DIType) where
  decodeM diTy = do
    sId <- liftIO $ FFI.getMetadataClassId (FFI.upCast diTy)
    case sId of
      [mdSubclassIdP|DIBasicType|] ->
        A.DIBasicType <$> decodeM (castPtr diTy :: Ptr FFI.DIBasicType)
      [mdSubclassIdP|DICompositeType|] ->
        A.DICompositeType <$> decodeM (castPtr diTy :: Ptr FFI.DICompositeType)
      [mdSubclassIdP|DIDerivedType|]    -> A.DIDerivedType <$> decodeM (castPtr diTy :: Ptr FFI.DIDerivedType)
      [mdSubclassIdP|DISubroutineType|] -> A.DISubroutineType <$> decodeM (castPtr diTy :: Ptr FFI.DISubroutineType)
      _ -> throwM (DecodeException ("Unknown subclass id for DIType: " <> show sId))

instance DecodeM DecodeAST A.DIBasicType (Ptr FFI.DIBasicType) where
  decodeM diTy = do
    tag <- decodeM =<< liftIO (FFI.getTag (FFI.upCast diTy))
    size <- liftIO (FFI.getTypeSizeInBits (FFI.upCast diTy))
    align <- liftIO (FFI.getTypeAlignInBits (FFI.upCast diTy))
    name <- decodeM =<< liftIO (FFI.getTypeName (FFI.upCast diTy))
    encoding <- decodeM =<< liftIO (FFI.getBasicTypeEncoding diTy)
    flags <- decodeM =<< liftIO (FFI.getTypeFlags (castPtr diTy :: Ptr FFI.DIType))
    pure (A.BasicType name size align encoding tag flags)

instance EncodeM EncodeAST A.DIBasicType (Ptr FFI.DIBasicType) where
  encodeM A.BasicType {..} = do
    tag <- encodeM tag
    name <- encodeM name
    encoding <- encodeM encoding
    flags <- encodeM flags
    Context c <- gets encodeStateContext
    liftIO (FFI.getDIBasicType c tag name sizeInBits alignInBits encoding flags)


instance EncodeM EncodeAST A.DISubroutineType (Ptr FFI.DISubroutineType) where
  encodeM A.SubroutineType {..} = do
    flags <- encodeM flags
    types <- encodeM typeArray
    Context c <- gets encodeStateContext
    liftIO (FFI.getDISubroutineType c flags cc types)

instance DecodeM DecodeAST A.DISubroutineType (Ptr FFI.DISubroutineType) where
  decodeM p = do
    flags <- decodeM =<< liftIO (FFI.getTypeFlags (FFI.upCast p))
    cc <-  liftIO (FFI.getSubroutineCC p)
    arr <- decodeM =<< liftIO (FFI.getSubroutineTypeArray p)
    pure A.SubroutineType
      { A.flags = flags
      , A.cc = cc
      , A.typeArray = arr
      }

instance EncodeM EncodeAST A.DIDerivedType (Ptr FFI.DIDerivedType) where
  encodeM A.DerivedType {..} = do
    tag <- encodeM tag
    name <- encodeM name
    file <- encodeM file
    line <- encodeM line
    scope <- encodeM scope
    type' <- encodeM baseType
    (addrSpace, addrSpacePresent) <- encodeM addressSpace
    flags <- encodeM flags
    Context c <- gets encodeStateContext
    FFI.upCast <$> liftIO (FFI.getDIDerivedType c tag name file line scope type' sizeInBits alignInBits offsetInBits addrSpace addrSpacePresent flags)

instance DecodeM DecodeAST A.DIDerivedType (Ptr FFI.DIDerivedType) where
  decodeM diTy = do
    let diTy' = FFI.upCast diTy
    name <- decodeM =<< liftIO (FFI.getTypeName diTy')
    file   <- decodeM =<< liftIO (FFI.getScopeFile (FFI.upCast diTy))
    scope  <- decodeM =<< liftIO (FFI.getScopeScope (FFI.upCast diTy))
    ty <- decodeM =<< liftIO (FFI.getDerivedBaseType diTy')
    line     <- liftIO (FFI.getTypeLine diTy')
    size     <- liftIO (FFI.getTypeSizeInBits diTy')
    align    <- liftIO (FFI.getTypeAlignInBits diTy')
    offset   <- liftIO (FFI.getTypeOffsetInBits diTy')
    tag      <- decodeM =<< liftIO (FFI.getTag (FFI.upCast diTy))
    flags <- decodeM =<< liftIO (FFI.getTypeFlags diTy')
    addressSpace <- decodeOptional (FFI.getDerivedAddressSpace diTy')
    pure A.DerivedType
      { A.tag = tag
      , A.name = name
      , A.file = file
      , A.line = line
      , A.scope = scope
      , A.baseType = ty
      , A.sizeInBits = size
      , A.alignInBits = align
      , A.offsetInBits = offset
      , A.addressSpace = addressSpace
      , A.flags = flags
      }

genCodingInstance [t|A.DerivedTypeTag|] ''FFI.DwTag
  [ (FFI.DwTag_typedef, A.Typedef)
  , (FFI.DwTag_pointer_type, A.PointerType)
  , (FFI.DwTag_ptr_to_member_type, A.PtrToMemberType)
  , (FFI.DwTag_reference_type, A.ReferenceType)
  , (FFI.DwTag_rvalue_reference_type, A.RValueReferenceType)
  , (FFI.DwTag_const_type, A.ConstType)
  , (FFI.DwTag_volatile_type, A.VolatileType)
  , (FFI.DwTag_restrict_type, A.RestrictType)
  , (FFI.DwTag_atomic_type, A.AtomicType)
  , (FFI.DwTag_member, A.Member)
  , (FFI.DwTag_inheritance, A.Inheritance)
  , (FFI.DwTag_friend, A.Friend)
  ]

instance EncodeM EncodeAST A.DIVariable (Ptr FFI.DIVariable) where
  encodeM (A.DIGlobalVariable v) = do
    ptr <- encodeM v
    pure (FFI.upCast (ptr :: Ptr FFI.DIGlobalVariable))
  encodeM (A.DILocalVariable v) = do
    ptr <- encodeM v
    pure (FFI.upCast (ptr :: Ptr FFI.DILocalVariable))

instance DecodeM DecodeAST A.DIVariable (Ptr FFI.DIVariable) where
  decodeM p = do
    sId <- liftIO $ FFI.getMetadataClassId (FFI.upCast p)
    case sId of
      [mdSubclassIdP|DIGlobalVariable|] ->
        A.DIGlobalVariable <$> decodeM (castPtr p :: Ptr FFI.DIGlobalVariable)
      [mdSubclassIdP|DILocalVariable|] ->
        A.DILocalVariable <$> decodeM (castPtr p :: Ptr FFI.DILocalVariable)
      _ -> throwM (DecodeException ("Unknown subclass id for DIVariable: " <> show sId))

instance DecodeM DecodeAST A.DIGlobalVariable (Ptr FFI.DIGlobalVariable) where
  decodeM p = do
    name <- decodeM =<< liftIO (FFI.getDIVariableName (FFI.upCast p))
    scope <- decodeM =<< liftIO (FFI.getDIVariableScope (FFI.upCast p))
    file <- decodeM =<< liftIO (FFI.getDIVariableFile (FFI.upCast p))
    line <- decodeM =<< liftIO (FFI.getDIVariableLine (FFI.upCast p))
    type' <- decodeM =<< liftIO (FFI.getDIVariableType (FFI.upCast p))
    align <- liftIO (FFI.getDIVariableAlignInBits (FFI.upCast p))
    linkageName <- decodeM =<< liftIO (FFI.getDIGlobalVariableLinkageName p)
    local <- decodeM =<< liftIO (FFI.getDIGlobalVariableLocal p)
    definition <- decodeM =<< liftIO (FFI.getDIGlobalVariableDefinition p)
    decl <- decodeM =<< liftIO (FFI.getDIGlobalVariableStaticDataMemberDeclaration p)
    pure A.GlobalVariable
      { A.name = name
      , A.scope = scope
      , A.file = file
      , A.line = line
      , A.type' = type'
      , A.linkageName = linkageName
      , A.local = local
      , A.definition = definition
      , A.staticDataMemberDeclaration = decl
      , A.templateParams = []
      , A.alignInBits = align
      }

instance EncodeM EncodeAST A.DIGlobalVariable (Ptr FFI.DIGlobalVariable) where
  encodeM A.GlobalVariable {..} = do
    name <- encodeM name
    scope <- encodeM scope
    file <- encodeM file
    line <- encodeM line
    type' <- encodeM type'
    linkageName <- encodeM linkageName
    local <- encodeM local
    definition <- encodeM definition
    dataMemberDeclaration <- encodeM staticDataMemberDeclaration
    templateParams <- encodeM templateParams
    Context c <- gets encodeStateContext
    liftIO (FFI.getDIGlobalVariable c scope name linkageName file line type' local definition dataMemberDeclaration templateParams alignInBits)

instance DecodeM DecodeAST A.DILocalVariable (Ptr FFI.DILocalVariable) where
  decodeM p = do
    name <- decodeM =<< liftIO (FFI.getDIVariableName (FFI.upCast p))
    scope <- decodeM =<< liftIO (FFI.getDIVariableScope (FFI.upCast p))
    file <- decodeM =<< liftIO (FFI.getDIVariableFile (FFI.upCast p))
    line <- decodeM =<< liftIO (FFI.getDIVariableLine (FFI.upCast p))
    type' <- decodeM =<< liftIO (FFI.getDIVariableType (FFI.upCast p))
    align <- liftIO (FFI.getDIVariableAlignInBits (FFI.upCast p))
    arg <- liftIO (FFI.getDILocalVariableArg p)
    flags <- decodeM =<< liftIO (FFI.getDILocalVariableFlags p)
    pure A.LocalVariable
      { A.file = file
      , A.scope = scope
      , A.name = name
      , A.line = line
      , A.arg = arg
      , A.flags = flags
      , A.type' = type'
      , A.alignInBits = align
      }

instance EncodeM EncodeAST A.DILocalVariable (Ptr FFI.DILocalVariable) where
  encodeM A.LocalVariable {..} = do
    name <- encodeM name
    scope <- encodeM scope
    file <- encodeM file
    type' <- encodeM type'
    flags <- encodeM flags
    Context c <- gets encodeStateContext
    FFI.upCast <$> liftIO (FFI.getDILocalVariable c scope name file line type' arg flags alignInBits)

genCodingInstance [t|A.TemplateValueParameterTag|] ''FFI.DwTag
  [ (FFI.DwTag_template_value_parameter, A.TemplateValueParameter)
  , (FFI.DwTag_GNU_template_template_param, A.GNUTemplateTemplateParam)
  , (FFI.DwTag_GNU_template_parameter_pack, A.GNUTemplateParameterPack)
  ]

instance EncodeM EncodeAST A.DITemplateParameter (Ptr FFI.DITemplateParameter) where
  encodeM p = do
    name' <- encodeM (A.name (p :: A.DITemplateParameter)) :: EncodeAST (Ptr FFI.MDString)
    ty <- encodeM (A.type' (p :: A.DITemplateParameter))
    Context c <- gets encodeStateContext
    case p of
      A.DITemplateTypeParameter {} ->
        FFI.upCast <$> liftIO (FFI.getDITemplateTypeParameter c name' ty)
      A.DITemplateValueParameter {..} -> do
        tag <- encodeM tag
        value <- encodeM value
        FFI.upCast <$> liftIO (FFI.getDITemplateValueParameter c name' ty tag value)

instance DecodeM DecodeAST A.DITemplateParameter (Ptr FFI.DITemplateParameter) where
  decodeM p = do
    sId <- liftIO (FFI.getMetadataClassId (FFI.upCast p))
    name <- decodeM =<< liftIO (FFI.getDITemplateParameterName p)
    ty <- decodeM =<< liftIO (FFI.getDITemplateParameterType p)
    case sId of
      [mdSubclassIdP|DITemplateTypeParameter|] ->
        pure (A.DITemplateTypeParameter name ty)
      [mdSubclassIdP|DITemplateValueParameter|] -> do
        value <- decodeM =<< liftIO (FFI.getDITemplateValueParameterValue (castPtr p))
        tag <- decodeM =<< liftIO (FFI.getTag (FFI.upCast p))
        pure (A.DITemplateValueParameter name ty value tag)
      _ -> throwM (DecodeException ("Unknown subclass id for DITemplateParameter: " <> show sId))

genCodingInstance [t|A.Virtuality|] ''FFI.DwVirtuality
  [ (FFI.DwVirtuality_none, A.NoVirtuality)
  , (FFI.DwVirtuality_virtual, A.Virtual)
  , (FFI.DwVirtuality_pure_virtual, A.PureVirtual)
  ]

instance DecodeM DecodeAST A.DISubprogram (Ptr FFI.DISubprogram) where
  decodeM p = do
    name  <- getByteStringFromFFI FFI.getScopeName (FFI.upCast p)
    linkageName <- decodeM =<< liftIO (FFI.getDISubprogramLinkageName p)
    file  <- decodeM =<< liftIO (FFI.getScopeFile (FFI.upCast p))
    scope <- decodeM =<< liftIO (FFI.getScopeScope (FFI.upCast p))
    line <- decodeM =<< liftIO (FFI.getDISubprogramLine p)
    virtuality <- decodeM =<< liftIO (FFI.getDISubprogramVirtuality p)
    virtualIndex <- decodeM =<< liftIO (FFI.getDISubprogramVirtualIndex p)
    scopeLine <- decodeM =<< liftIO (FFI.getDISubprogramScopeLine p)
    optimized <- decodeM =<< liftIO (FFI.getDISubprogramIsOptimized p)
    definition <- decodeM =<< liftIO (FFI.getDISubprogramIsDefinition p)
    localToUnit <- decodeM =<< liftIO (FFI.getDISubprogramLocalToUnit p)
    thisAdjustment <- liftIO (FFI.getDISubprogramThisAdjustment p)
    flags <- decodeM =<< liftIO (FFI.getDISubprogramFlags p)
    type' <- decodeM =<< liftIO (FFI.getDISubprogramType p)
    containingType <- decodeM =<< liftIO (FFI.getDISubprogramContainingType p)
    unit <- decodeM =<< liftIO (FFI.getDISubprogramUnit p)
    templateParams <- decodeM =<< liftIO (FFI.getDISubprogramTemplateParams p)
    retainedNodes <- decodeM =<< liftIO (FFI.getDISubprogramRetainedNodes p)
    thrownTypes <- decodeM =<< liftIO (FFI.getDISubprogramThrownTypes p)
    decl <- decodeM =<< liftIO (FFI.getDISubprogramDeclaration p)
    pure A.Subprogram
      { A.name = name
      , A.linkageName = linkageName
      , A.scope = scope
      , A.file = file
      , A.line = line
      , A.type' = type'
      , A.definition = definition
      , A.scopeLine = scopeLine
      , A.containingType = containingType
      , A.virtuality = virtuality
      , A.virtualityIndex = virtualIndex
      , A.flags = flags
      , A.optimized = optimized
      , A.unit = unit
      , A.templateParams = templateParams
      , A.declaration = decl
      , A.retainedNodes = retainedNodes
      , A.thrownTypes = thrownTypes
      , A.localToUnit = localToUnit
      , A.thisAdjustment = thisAdjustment
      }

instance EncodeM EncodeAST A.DISubprogram (Ptr FFI.DISubprogram) where
  encodeM A.Subprogram {..} = do
    scope <- encodeM scope
    name <- encodeM name
    linkageName <- encodeM linkageName
    file <- encodeM file
    line <- encodeM line
    type' <- encodeM type'
    localToUnit <- encodeM localToUnit
    definition <- encodeM definition
    scopeLine <- encodeM scopeLine
    containingType <- encodeM containingType
    virtuality <- encodeM virtuality
    virtualityIndex <- encodeM virtualityIndex
    flags <- encodeM flags
    optimized <- encodeM optimized
    unit <- encodeM unit
    templateParams <- encodeM templateParams
    declaration <- encodeM declaration
    retainedNodes <- encodeM retainedNodes
    thrownTypes <- encodeM thrownTypes
    Context c <- gets encodeStateContext
    FFI.upCast <$> liftIO
      (FFI.getDISubprogram
        c scope name
        linkageName file line
        type' localToUnit definition scopeLine
        containingType virtuality virtualityIndex
        thisAdjustment flags optimized
        unit templateParams declaration
        retainedNodes thrownTypes)

instance DecodeM DecodeAST A.DILocalScope (Ptr FFI.DILocalScope) where
  decodeM ls = do
    sId <- liftIO $ FFI.getMetadataClassId (FFI.upCast ls)
    case sId of
      [mdSubclassIdP|DISubprogram|] -> A.DISubprogram <$> decodeM (castPtr ls :: Ptr FFI.DISubprogram)
      [mdSubclassIdP|DILexicalBlock|] -> A.DILexicalBlockBase <$> decodeM (castPtr ls :: Ptr FFI.DILexicalBlockBase)
      [mdSubclassIdP|DILexicalBlockFile|] -> A.DILexicalBlockBase <$> decodeM (castPtr ls :: Ptr FFI.DILexicalBlockBase)
      _ -> throwM (DecodeException ("Unknown subclass id for DILocalScope: " <> show sId))

instance EncodeM EncodeAST A.DILocalScope (Ptr FFI.DILocalScope) where
  encodeM (A.DISubprogram sp) = do
    ptr <- encodeM sp
    pure (FFI.upCast (ptr :: Ptr FFI.DISubprogram))
  encodeM (A.DILexicalBlockBase b) = do
    ptr <- encodeM b
    pure (FFI.upCast (ptr :: Ptr FFI.DILexicalBlockBase))

instance DecodeM DecodeAST A.DILexicalBlockBase (Ptr FFI.DILexicalBlockBase) where
  decodeM p = do
    sId <- liftIO (FFI.getMetadataClassId (FFI.upCast p))
    case sId of
      [mdSubclassIdP|DILexicalBlock|] -> do
        scope <- decodeM =<< liftIO (FFI.getLexicalBlockScope p)
        file  <- decodeM =<< liftIO (FFI.getScopeFile (FFI.upCast p))
        line  <- liftIO (FFI.getLexicalBlockLine p)
        col   <- liftIO (FFI.getLexicalBlockColumn p)
        pure (A.DILexicalBlock scope file line col)
      [mdSubclassIdP|DILexicalBlockFile|] -> do
        scope <- decodeM =<< liftIO (FFI.getLexicalBlockScope p)
        file  <- decodeM =<< liftIO (FFI.getScopeFile (FFI.upCast p))
        disc  <- liftIO (FFI.getLexicalBlockFileDiscriminator p)
        pure (A.DILexicalBlockFile scope file disc)
      _ -> throwM (DecodeException ("Unknown subclass id for DILexicalBlockBase: " <> show sId))

instance EncodeM EncodeAST A.DILexicalBlockBase (Ptr FFI.DILexicalBlockBase) where
  encodeM A.DILexicalBlock {..} = do
    scope <- encodeM scope
    file <- encodeM file
    Context c <- gets encodeStateContext
    FFI.upCast <$> liftIO (FFI.getDILexicalBlock c scope file line column)
  encodeM A.DILexicalBlockFile {..} = do
    scope <- encodeM scope
    file <- encodeM file
    Context c <- gets encodeStateContext
    FFI.upCast <$> liftIO (FFI.getDILexicalBlockFile c scope file discriminator)

instance DecodeM DecodeAST A.CallableOperand (Ptr FFI.Value) where
  decodeM v = do
    ia <- liftIO $ FFI.isAInlineAsm v
    if ia /= nullPtr
     then Left <$> decodeM ia
     else Right <$> decodeM v

instance EncodeM EncodeAST A.Operand (Ptr FFI.Value) where
  encodeM (A.ConstantOperand c) = (FFI.upCast :: Ptr FFI.Constant -> Ptr FFI.Value) <$> encodeM c
  encodeM (A.LocalReference t n) = do
    lv <- refer encodeStateLocals n $ do
      lv <- do
        n <- encodeM n
        t <- encodeM t
        v <- liftIO $ FFI.createArgument t n
        return $ ForwardValue v
      modify $ \s -> s { encodeStateLocals = Map.insert n lv $ encodeStateLocals s }
      return lv
    return $ case lv of DefinedValue v -> v; ForwardValue v -> v
  encodeM (A.MetadataOperand md) = do
    md' <- encodeM md
    Context c <- gets encodeStateContext
    liftIO $ FFI.upCast <$> FFI.metadataOperand c md'

instance EncodeM EncodeAST A.Metadata (Ptr FFI.Metadata) where
  encodeM (A.MDString s) = do
    Context c <- gets encodeStateContext
    s <- encodeM s
    FFI.upCast <$> liftIO (FFI.getMDString c s)
  encodeM (A.MDNode mdn) = (FFI.upCast :: Ptr FFI.MDNode -> Ptr FFI.Metadata) <$> encodeM mdn
  encodeM (A.MDValue v) = do
     v <- encodeM v
     FFI.upCast <$> liftIO (FFI.mdValue v)

instance EncodeM EncodeAST A.CallableOperand (Ptr FFI.Value) where
  encodeM (Right o) = encodeM o
  encodeM (Left i) = (FFI.upCast :: Ptr FFI.InlineAsm -> Ptr FFI.Value) <$> encodeM i

instance EncodeM EncodeAST A.MDNode (Ptr FFI.MDNode) where
  encodeM (A.MDTuple ops) = scopeAnyCont $ do
    Context c <- gets encodeStateContext
    ops <- encodeM ops
    FFI.upCast <$> liftIO (FFI.getMDTuple c ops)
  encodeM (A.DINode n) = do
    ptr <- encodeM n
    pure (FFI.upCast (ptr :: Ptr FFI.DINode))
  encodeM (A.DILocation l) = FFI.upCast <$> (encodeM l :: EncodeAST (Ptr FFI.DILocation))
  encodeM (A.DIExpression e) = FFI.upCast <$> (encodeM e :: EncodeAST (Ptr FFI.DIExpression))
  encodeM (A.DIGlobalVariableExpression e) =
    FFI.upCast <$> (encodeM e :: EncodeAST (Ptr FFI.DIGlobalVariableExpression))
  encodeM (A.DIMacroNode n) = FFI.upCast <$> (encodeM n :: EncodeAST (Ptr FFI.DIMacroNode))

instance EncodeM EncodeAST A.DILocation (Ptr FFI.DILocation) where
  encodeM A.Location {..} = do
    Context c <- gets encodeStateContext
    scopePtr <- encodeM scope
    liftIO (FFI.getDILocation c line column scopePtr)

instance DecodeM DecodeAST A.DILocation (Ptr FFI.DILocation) where
  decodeM p = do
    line <- liftIO (FFI.getDILocationLine p)
    col  <- liftIO (FFI.getDILocationColumn p)
    scope <-  decodeM =<< liftIO (FFI.getDILocationScope p)
    pure (A.Location line col scope)

instance (MonadIO m, MonadState EncodeState m, MonadAnyCont IO m, EncodeM m a (Ptr a'), FFI.DescendentOf FFI.Metadata a') => EncodeM m [a] (FFI.TupleArray a') where
  encodeM [] = pure (FFI.TupleArray nullPtr)
  encodeM els = do
    (numEls, elsPtr) <- encodeM els
    Context c <- gets encodeStateContext
    FFI.TupleArray <$> liftIO (FFI.getMDTuple c (numEls, castPtr (elsPtr :: Ptr (Ptr a'))))

instance (MonadIO m, MonadAnyCont IO m, DecodeM m a (Ptr a')) => DecodeM m [a] (FFI.TupleArray a') where
  decodeM (FFI.TupleArray p)
    | p == nullPtr = pure []
    | otherwise = decodeArray FFI.getMDNodeNumOperands getOperand (FFI.upCast p)
    where getOperand md i = (castPtr <$> FFI.getMDNodeOperand md i) :: IO (Ptr a')

encodeDWOp :: A.DWOp -> [Word64]
encodeDWOp op =
  case op of
    A.DW_OP_And -> [FFI.DwOp_and]
    A.DW_OP_Bregx -> [FFI.DwOp_bregx]
    A.DW_OP_ConstU arg -> [FFI.DwOp_constu, arg]
    A.DW_OP_Deref -> [FFI.DwOp_deref]
    A.DW_OP_Div -> [FFI.DwOp_div]
    A.DW_OP_Dup -> [FFI.DwOp_dup]
    A.DwOpFragment (A.DW_OP_LLVM_Fragment offset size) -> [FFI.DwOp_LLVM_fragment, offset, size]
    A.DW_OP_Lit0 -> [FFI.DwOp_lit0]
    A.DW_OP_Minus -> [FFI.DwOp_minus]
    A.DW_OP_Mod -> [FFI.DwOp_mod]
    A.DW_OP_Mul -> [FFI.DwOp_mul]
    A.DW_OP_Not -> [FFI.DwOp_not]
    A.DW_OP_Or -> [FFI.DwOp_or]
    A.DW_OP_Plus -> [FFI.DwOp_plus]
    A.DW_OP_PlusUConst arg -> [FFI.DwOp_plus_uconst, arg]
    A.DW_OP_PushObjectAddress -> [FFI.DwOp_push_object_address]
    A.DW_OP_Shl -> [FFI.DwOp_shl]
    A.DW_OP_Shra -> [FFI.DwOp_shra]
    A.DW_OP_Shr -> [FFI.DwOp_shr]
    A.DW_OP_StackValue -> [FFI.DwOp_stack_value]
    A.DW_OP_Swap -> [FFI.DwOp_swap]
    A.DW_OP_XDeref -> [FFI.DwOp_xderef]
    A.DW_OP_Xor -> [FFI.DwOp_xor]

instance DecodeM DecodeAST [Maybe A.Metadata] (Ptr FFI.MDNode) where
  decodeM p = decodeArray FFI.getMDNodeNumOperands FFI.getMDNodeOperand p

instance DecodeM DecodeAST A.Operand (Ptr FFI.MDValue) where
  decodeM = decodeM <=< liftIO . FFI.getMDValue

instance DecodeM DecodeAST A.Metadata (Ptr FFI.MetadataAsVal) where
  decodeM = decodeM <=< liftIO . FFI.getMetadataOperand

genCodingInstance [t|A.DIMacroInfo|] ''FFI.Macinfo [ (FFI.DW_Macinfo_Define, A.Define), (FFI.DW_Macinfo_Undef, A.Undef) ]

decodeMDNode :: Ptr FFI.MDNode -> DecodeAST (Either String A.MDNode)
decodeMDNode p = scopeAnyCont $ do
  sId <- liftIO $ FFI.getMetadataClassId p
  case sId of
      [mdSubclassIdP|MDTuple|] -> liftM Right $ A.MDTuple <$> decodeM p
      [mdSubclassIdP|DIExpression|] ->
        liftM Right $ A.DIExpression <$> decodeM (castPtr p :: Ptr FFI.DIExpression)
      [mdSubclassIdP|DIGlobalVariableExpression|] ->
        liftM Right $ A.DIGlobalVariableExpression <$> decodeM (castPtr p :: Ptr FFI.DIGlobalVariableExpression)
      [mdSubclassIdP|DILocation|] -> liftM Right $ A.DILocation <$> decodeM (castPtr p :: Ptr FFI.DILocation)
      [mdSubclassIdP|DIMacro|] -> liftM Right $ A.DIMacroNode <$> decodeM (castPtr p :: Ptr FFI.DIMacroNode)
      [mdSubclassIdP|DIMacroFile|] -> liftM Right $ A.DIMacroNode <$> decodeM (castPtr p :: Ptr FFI.DIMacroNode)
      _ -> do
        decoded <- decodeM (castPtr p :: Ptr FFI.DINode)
        case decoded of
          (Right din) -> liftM Right $ pure $ A.DINode din
          (Left err) -> pure $ Left err

instance DecodeM DecodeAST A.DIMacroNode (Ptr FFI.DIMacroNode) where
  decodeM p = do
    sId <- liftIO $ FFI.getMetadataClassId (FFI.upCast p)
    case sId of
      [mdSubclassIdP|DIMacro|] -> do
        let p' = castPtr p :: Ptr FFI.DIMacro
        macInfo <- decodeM =<< liftIO (FFI.getDIMacroMacinfo p')
        line <- liftIO (FFI.getDIMacroLine p')
        name <- decodeM =<< liftIO (FFI.getDIMacroName p')
        value <- decodeM =<< liftIO (FFI.getDIMacroValue p')
        pure (A.DIMacro macInfo line name value)
      [mdSubclassIdP|DIMacroFile|] -> do
        let p' = castPtr p :: Ptr FFI.DIMacroFile
        line <- liftIO (FFI.getDIMacroFileLine p')
        file <- decodeM =<< liftIO (FFI.getDIMacroFileFile p')
        elements <- decodeArray FFI.getDIMacroFileNumElements FFI.getDIMacroFileElement p'
        pure (A.DIMacroFile line file elements)
      _ -> throwM (DecodeException ("Unknown subclass id for DIMacroNode: " <> show sId))

instance EncodeM EncodeAST A.DIMacroNode (Ptr FFI.DIMacroNode) where
  encodeM A.DIMacro {..} = do
    macInfo <- encodeM info
    name <- encodeM name
    value <- encodeM value
    Context c <- gets encodeStateContext
    FFI.upCast <$> liftIO (FFI.getDIMacro c macInfo line name value)
  encodeM A.DIMacroFile {..} = do
    file <- encodeM file
    elements <- encodeM elements
    Context c <- gets encodeStateContext
    FFI.upCast <$> liftIO (FFI.getDIMacroFile c line file elements)

instance EncodeM EncodeAST A.DIExpression (Ptr FFI.DIExpression) where
  encodeM A.Expression {..} = do
    ops <- encodeM (concatMap encodeDWOp operands)
    Context c <- gets encodeStateContext
    FFI.upCast <$> liftIO (FFI.getDIExpression c ops)

instance DecodeM DecodeAST A.DIExpression (Ptr FFI.DIExpression) where
  decodeM diExpr = do
    numElems <- liftIO (FFI.getDIExpressionNumElements diExpr)
    let go i
          | i >= numElems = pure []
          | otherwise = do
              op <- FFI.getDIExpressionElement diExpr i
              case op of
                FFI.DwOp_LLVM_fragment -> do
                  expectElems "LLVM_fragment" i 2
                  offset <- FFI.getDIExpressionElement diExpr (i + 1)
                  size <- FFI.getDIExpressionElement diExpr (i + 2)
                  (A.DwOpFragment (A.DW_OP_LLVM_Fragment offset size) :) <$> go (i + 3)
                FFI.DwOp_stack_value -> (A.DW_OP_StackValue :) <$> go (i + 1)
                FFI.DwOp_swap -> (A.DW_OP_Swap :) <$> go (i + 1)
                FFI.DwOp_constu -> do
                  expectElems "constu" i 1
                  arg <- FFI.getDIExpressionElement diExpr (i + 1)
                  (A.DW_OP_ConstU arg:) <$> go (i + 2)
                FFI.DwOp_lit0 -> (A.DW_OP_Lit0 :) <$> go (i + 1)
                FFI.DwOp_plus_uconst -> do
                  expectElems "uconst" i 1
                  arg <- FFI.getDIExpressionElement diExpr (i + 1)
                  (A.DW_OP_PlusUConst arg:) <$> go (i + 2)
                FFI.DwOp_plus -> (A.DW_OP_Plus :) <$> go (i + 1)
                FFI.DwOp_minus -> (A.DW_OP_Minus :) <$> go (i + 1)
                FFI.DwOp_mul -> (A.DW_OP_Mul :) <$> go (i + 1)
                FFI.DwOp_div -> (A.DW_OP_Div :) <$> go (i + 1)
                FFI.DwOp_mod -> (A.DW_OP_Mod :) <$> go (i + 1)
                FFI.DwOp_not -> (A.DW_OP_Not :) <$> go (i + 1)
                FFI.DwOp_or -> (A.DW_OP_Or :) <$> go (i + 1)
                FFI.DwOp_xor -> (A.DW_OP_Xor :) <$> go (i + 1)
                FFI.DwOp_and -> (A.DW_OP_And :) <$> go (i + 1)
                FFI.DwOp_shr -> (A.DW_OP_Shr :) <$> go (i + 1)
                FFI.DwOp_shra -> (A.DW_OP_Shra :) <$> go (i + 1)
                FFI.DwOp_shl -> (A.DW_OP_Shl :) <$> go (i + 1)
                FFI.DwOp_dup -> (A.DW_OP_Dup :) <$> go (i + 1)
                FFI.DwOp_deref -> (A.DW_OP_Deref :) <$> go (i + 1)
                FFI.DwOp_xderef -> (A.DW_OP_XDeref :) <$> go (i + 1)
                FFI.DwOp_bregx -> (A.DW_OP_Bregx :) <$> go (i + 1)
                FFI.DwOp_push_object_address -> (A.DW_OP_PushObjectAddress :) <$> go (i + 1)
                _ -> throwM (DecodeException ("Unknown DW_OP " <> show op))
        expectElems name i n =
          when (i + n >= numElems)
               (throwM (DecodeException ("Expected " <> show n <> " elements following DW_OP_" <>
                                         name <> " but got " <> show (numElems - i - 1))))
    liftIO (A.Expression <$> go 0)

instance EncodeM EncodeAST A.DIGlobalVariableExpression (Ptr FFI.DIGlobalVariableExpression) where
  encodeM A.GlobalVariableExpression {..} = do
    var <- encodeM var
    expr <- encodeM expr
    Context c <- gets encodeStateContext
    liftIO (FFI.getDIGlobalVariableExpression c var expr)

instance DecodeM DecodeAST A.DIGlobalVariableExpression (Ptr FFI.DIGlobalVariableExpression) where
  decodeM p = do
    var <- decodeM =<< liftIO (FFI.getDIGlobalVariableExpressionVariable p)
    expr <- decodeM =<< liftIO (FFI.getDIGlobalVariableExpressionExpression p)
    pure (A.GlobalVariableExpression var expr)

genCodingInstance [t|A.ImportedEntityTag|] ''FFI.DwTag
  [ (FFI.DwTag_imported_module, A.ImportedModule)
  , (FFI.DwTag_imported_declaration, A.ImportedDeclaration)
  ]

instance EncodeM EncodeAST A.DIImportedEntity (Ptr FFI.DIImportedEntity) where
  encodeM A.ImportedEntity {..} = do
    tag <- encodeM tag
    scope <- encodeM scope
    entity <- encodeM entity
    file <- encodeM file
    name <- encodeM name
    Context c <- gets encodeStateContext
    liftIO (FFI.getDIImportedEntity c tag scope entity file line name)

instance DecodeM DecodeAST A.DIImportedEntity (Ptr FFI.DIImportedEntity) where
  decodeM e = do
    tag <- decodeM =<< liftIO (FFI.getTag (FFI.upCast e))
    scope <- decodeM =<< liftIO (FFI.getDIImportedEntityScope e)
    entity :: Either String A.DINode <- decodeM =<< liftIO (FFI.getDIImportedEntityEntity e)
    file <- decodeM =<< liftIO (FFI.getDIImportedEntityFile e)
    name <- decodeM =<< liftIO (FFI.getDIImportedEntityName e)
    line <- liftIO (FFI.getDIImportedEntityLine e)
    case entity of
      (Right din) -> pure (A.ImportedEntity tag name scope (Just $ A.MDInline din) file line)
      (Left _) -> pure (A.ImportedEntity tag name scope Nothing file line)

instance EncodeM EncodeAST A.DIObjCProperty (Ptr FFI.DIObjCProperty) where
  encodeM A.ObjCProperty {..} = do
    name <- encodeM name
    file <- encodeM file
    getterName <- encodeM getterName
    setterName <- encodeM setterName
    type' <- encodeM type'
    Context c <- gets encodeStateContext
    liftIO (FFI.getDIObjCProperty c name file line getterName setterName attributes type')

instance DecodeM DecodeAST A.DIObjCProperty (Ptr FFI.DIObjCProperty) where
  decodeM p = do
    name <- decodeM =<< liftIO (FFI.getDIObjCPropertyName p)
    file <- decodeM =<< liftIO (FFI.getDIObjCPropertyFile p)
    line <- liftIO (FFI.getDIObjCPropertyLine p)
    getterName <- decodeM =<< liftIO (FFI.getDIObjCPropertyGetterName p)
    setterName <- decodeM =<< liftIO (FFI.getDIObjCPropertySetterName p)
    attributes <- liftIO (FFI.getDIObjCPropertyAttributes p)
    type' <- decodeM =<< liftIO (FFI.getDIObjCPropertyType p)
    pure (A.ObjCProperty name file line getterName setterName attributes type')

instance {-# OVERLAPS #-} DecodeM DecodeAST (A.MDRef A.MDNode) (Ptr FFI.MDNode) where
  decodeM p = scopeAnyCont $ do
    sId <- liftIO $ FFI.getMetadataClassId p
    case sId of
      [mdSubclassIdP|DIExpression|] -> A.MDInline . A.DIExpression <$> decodeM (castPtr p :: Ptr FFI.DIExpression)
      _ -> A.MDRef <$> getMetadataNodeID p

instance (DecodeM DecodeAST a (Ptr b), FFI.DescendentOf FFI.MDNode b) => DecodeM DecodeAST (A.MDRef a) (Ptr b) where
  decodeM p = scopeAnyCont $
    A.MDRef <$> getMetadataNodeID (FFI.upCast p)

instance (EncodeM EncodeAST a (Ptr b), FFI.DescendentOf FFI.MDNode b) => EncodeM EncodeAST (A.MDRef a) (Ptr b) where
  encodeM (A.MDRef id) = castPtr <$> referMDNode id
  encodeM (A.MDInline m) = encodeM m

getMetadataDefinitions :: DecodeAST [A.Definition]
getMetadataDefinitions = fix $ \continue -> do
  mdntd <- takeMetadataNodeToDefine
  case mdntd of
    Nothing -> pure []
    Just (mid, p) -> do
      decoded <- decodeMDNode p
      case decoded of
        (Right mdn) ->
          (:)
            <$> (pure $ A.MetadataNodeDefinition mid mdn)
            <*> continue
        (Left _) -> continue
