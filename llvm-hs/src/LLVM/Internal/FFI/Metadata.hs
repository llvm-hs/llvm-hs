{-#
  LANGUAGE
  ForeignFunctionInterface
  #-}

module LLVM.Internal.FFI.Metadata where

import LLVM.Prelude

import Foreign.Ptr
import Foreign.C

import LLVM.Internal.FFI.Context
import LLVM.Internal.FFI.PtrHierarchy
import LLVM.Internal.FFI.LLVMCTypes

newtype DIFlags = DIFlags Word32
  deriving (Show, Eq)

data DITemplateParameterArray

-- | A 'TupleArray a' stores an array of elements of type 'Ptr a' using an 'MDTuple'.
newtype TupleArray a = TupleArray (Ptr MDTuple)

foreign import ccall unsafe "LLVM_Hs_IsAMDString" isAMDString ::
  Ptr Metadata -> IO (Ptr MDString)

foreign import ccall unsafe "LLVM_Hs_IsAMDNode" isAMDNode ::
  Ptr Metadata -> IO (Ptr MDNode)

foreign import ccall unsafe "LLVM_Hs_IsAMDValue" isAMDValue ::
  Ptr Metadata -> IO (Ptr MDValue)

foreign import ccall unsafe "LLVM_Hs_IsAMetadataOperand" isAMetadataOperand ::
  Ptr Value -> IO (Ptr MetadataAsVal)

foreign import ccall unsafe "LLVM_Hs_GetMetadataClassId" getMetadataClassId ::
  Ptr MDNode -> IO (MDSubclassID)

-- DILocation

foreign import ccall unsafe "LLVM_Hs_DILocation_GetLine" getDILocationLine ::
  Ptr DILocation -> IO Word32

foreign import ccall unsafe "LLVM_Hs_DILocation_GetColumn" getDILocationColumn ::
  Ptr DILocation -> IO Word16

foreign import ccall unsafe "LLVM_Hs_DILocation_GetScope" getDILocationScope ::
  Ptr DILocation -> IO (Ptr DILocalScope)

foreign import ccall unsafe "LLVM_Hs_Get_DILocation" getDILocation ::
  Ptr Context -> Word32 -> Word16 -> Ptr DILocalScope -> IO (Ptr DILocation)

foreign import ccall unsafe "LLVM_Hs_GetMDValue" getMDValue ::
  Ptr MDValue -> IO (Ptr Value)

foreign import ccall unsafe "LLVM_Hs_GetMetadataOperand" getMetadataOperand ::
  Ptr MetadataAsVal -> IO (Ptr Metadata)

foreign import ccall unsafe "LLVMGetMDKindIDInContext" getMDKindIDInContext' ::
  Ptr Context -> Ptr CChar -> CUInt -> IO MDKindID

getMDKindIDInContext :: Ptr Context -> (Ptr CChar, CUInt) -> IO MDKindID
getMDKindIDInContext ctx (c, n) = getMDKindIDInContext' ctx c n

foreign import ccall unsafe "LLVM_Hs_GetMDKindNames" getMDKindNames ::
  Ptr Context -> Ptr (Ptr CChar) -> Ptr CUInt -> CUInt -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_GetMDString" getMDString' ::
  Ptr Context -> CString -> CUInt -> IO (Ptr MDString)

getMDString :: Ptr Context -> (CString, CUInt) -> IO (Ptr MDString)
getMDString ctx (p, n) = getMDString' ctx p n

foreign import ccall unsafe "LLVM_Hs_MDValue" mdValue ::
  Ptr Value -> IO (Ptr MDValue)

foreign import ccall unsafe "LLVM_Hs_MetadataOperand" metadataOperand ::
  Ptr Context -> Ptr Metadata -> IO (Ptr Value)

foreign import ccall unsafe "LLVM_Hs_GetMDStringValue" getMDStringValue ::
  Ptr MDString -> Ptr CUInt -> IO CString

foreign import ccall unsafe "LLVM_Hs_Get_MDTuple" getMDTuple' ::
  Ptr Context -> Ptr (Ptr Metadata) -> CUInt -> IO (Ptr MDTuple)

getMDTuple :: Ptr Context -> (CUInt, Ptr (Ptr Metadata)) -> IO (Ptr MDTuple)
getMDTuple ctx (n, vs) = getMDTuple' ctx vs n

foreign import ccall unsafe "LLVM_Hs_CreateTemporaryMDNodeInContext" createTemporaryMDNodeInContext ::
  Ptr Context -> IO (Ptr MDNode)

foreign import ccall unsafe "LLVM_Hs_DestroyTemporaryMDNode" destroyTemporaryMDNode ::
  Ptr MDNode -> IO ()

foreign import ccall unsafe "LLVM_Hs_MDNode_GetNumOperands" getMDNodeNumOperands ::
  Ptr MDNode -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_MDNode_GetOperand" getMDNodeOperand ::
  Ptr MDNode -> CUInt -> IO (Ptr Metadata)

foreign import ccall unsafe "LLVM_Hs_GetNamedMetadataName" getNamedMetadataName ::
  Ptr NamedMetadata -> Ptr CUInt -> IO (Ptr CChar)

foreign import ccall unsafe "LLVM_Hs_GetNamedMetadataNumOperands" getNamedMetadataNumOperands ::
  Ptr NamedMetadata -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_GetNamedMetadataOperands" getNamedMetadataOperands ::
  Ptr NamedMetadata -> Ptr (Ptr MDNode) -> IO ()

foreign import ccall unsafe "LLVM_Hs_NamedMetadataAddOperands" namedMetadataAddOperands' ::
  Ptr NamedMetadata -> Ptr (Ptr MDNode) -> CUInt -> IO ()

foreign import ccall unsafe "LLVM_Hs_MetadataReplaceAllUsesWith" metadataReplaceAllUsesWith ::
  Ptr MDNode -> Ptr Metadata -> IO ()

namedMetadataAddOperands :: Ptr NamedMetadata -> (CUInt, Ptr (Ptr MDNode)) -> IO ()
namedMetadataAddOperands nm (n, vs) = namedMetadataAddOperands' nm vs n

-- DIEnumerator

foreign import ccall unsafe "LLVM_Hs_Get_DIEnumerator" getDIEnumerator ::
  Ptr Context -> Int64 -> LLVMBool -> Ptr MDString -> IO (Ptr DIEnumerator)

foreign import ccall unsafe "LLVM_Hs_DIEnumerator_GetValue" getDIEnumeratorValue ::
  Ptr DIEnumerator -> IO Int64

foreign import ccall unsafe "LLVM_Hs_DIEnumerator_GetIsUnsigned" getDIEnumeratorIsUnsigned ::
  Ptr DIEnumerator -> IO LLVMBool

foreign import ccall unsafe "LLVM_Hs_DIEnumerator_GetName" getDIEnumeratorName ::
  Ptr DIEnumerator -> IO (Ptr MDString)


foreign import ccall unsafe "LLVM_Hs_DIFileGetFilename" getFileFilename ::
  Ptr DIFile -> IO (Ptr MDString)

foreign import ccall unsafe "LLVM_Hs_DIFileGetDirectory" getFileDirectory ::
  Ptr DIFile -> IO (Ptr MDString)

foreign import ccall unsafe "LLVM_Hs_DIFileGetChecksum" getFileChecksumValue ::
  Ptr DIFile -> IO (Ptr MDString)

foreign import ccall unsafe "LLVM_Hs_DIFileGetChecksumKind" getFileChecksumKind ::
  Ptr DIFile -> IO ChecksumKind

foreign import ccall unsafe "LLVM_Hs_DIScope_GetName" getScopeName ::
  Ptr DIScope -> Ptr CUInt -> IO CString

foreign import ccall unsafe "LLVM_Hs_DITypeGetName" getTypeName ::
  Ptr DIType -> IO (Ptr MDString)

foreign import ccall unsafe "LLVM_Hs_DITypeGetAlignInBits" getTypeAlignInBits ::
  Ptr DIType -> IO Word32

foreign import ccall unsafe "LLVM_Hs_DITypeGetSizeInBits" getTypeSizeInBits ::
  Ptr DIType -> IO Word64

foreign import ccall unsafe "LLVM_Hs_DITypeGetOffsetInBits" getTypeOffsetInBits ::
  Ptr DIType -> IO Word64

foreign import ccall unsafe "LLVM_Hs_DINodeGetTag" getTag ::
  Ptr DINode -> IO DwTag

foreign import ccall unsafe "LLVM_Hs_DITypeGetLine" getTypeLine ::
  Ptr DIType -> IO Word32

foreign import ccall unsafe "LLVM_Hs_DITypeGetFlags" getTypeFlags ::
  Ptr DIType -> IO DIFlags

foreign import ccall unsafe "LLVM_Hs_DICompositeType_GetElements" getElements ::
  Ptr DICompositeType -> IO (Ptr MDTuple)

foreign import ccall unsafe "LLVM_Hs_DICompositeTypeGetVTableHolder" getVTableHolder ::
  Ptr DICompositeType -> IO (Ptr DIType)

foreign import ccall unsafe "LLVM_Hs_DICompositeTypeGetBaseType" getCompositeBaseType ::
  Ptr DICompositeType -> IO (Ptr DIType)

foreign import ccall unsafe "LLVM_Hs_DICompositeTypeGetRuntimeLang" getRuntimeLang ::
  Ptr DICompositeType -> IO Word16

foreign import ccall unsafe "LLVM_Hs_DICompositeType_GetTemplateParameters" getTemplateParams ::
  Ptr DICompositeType -> IO (TupleArray DITemplateParameter)

foreign import ccall unsafe "LLVM_Hs_DICompositeTypeGetIdentifier" getIdentifier ::
  Ptr DICompositeType -> IO (Ptr MDString)

foreign import ccall unsafe "LLVM_Hs_Get_DIArrayType" getDIArrayType ::
  Ptr Context -> TupleArray DISubrange -> Ptr DIType -> Word64 -> Word32 -> DIFlags -> IO (Ptr DICompositeType)

foreign import ccall unsafe "LLVM_Hs_Get_DIEnumerationType" getDIEnumerationType ::
  Ptr Context -> Ptr DIScope -> Ptr MDString -> Ptr DIFile -> Word32 -> Word64 -> Word32 -> TupleArray DIEnumerator -> Ptr DIType -> Ptr MDString -> IO (Ptr DICompositeType)

foreign import ccall unsafe "LLVM_Hs_Get_DIStructType" getDIStructType ::
  Ptr Context -> Ptr DIScope -> Ptr MDString -> Ptr DIFile ->
  Word32 -> Word64 -> Word32 -> DIFlags ->
  Ptr DIType -> TupleArray DIScope ->
  Word16 -> Ptr DIType -> Ptr MDString ->
  IO (Ptr DICompositeType)

foreign import ccall unsafe "LLVM_Hs_Get_DIUnionType" getDIUnionType ::
  Ptr Context -> Ptr DIScope -> Ptr MDString -> Ptr DIFile ->
  Word32 -> Word64 -> Word32 -> DIFlags ->
  TupleArray DIScope ->
  Word16 -> Ptr MDString ->
  IO (Ptr DICompositeType)

foreign import ccall unsafe "LLVM_Hs_Get_DIClassType" getDIClassType ::
  Ptr Context -> Ptr DIScope -> Ptr MDString -> Ptr DIFile ->
  Word32 -> Word64 -> Word32 -> DIFlags ->
  Ptr DIType -> TupleArray DIScope ->
  Ptr DIType -> TupleArray DITemplateParameter -> Ptr MDString ->
  IO (Ptr DICompositeType)

foreign import ccall unsafe "LLVM_Hs_Get_DINamespace" getDINamespace ::
  Ptr Context -> Ptr DIScope -> Ptr MDString -> LLVMBool -> IO (Ptr DINamespace)

foreign import ccall unsafe "LLVM_Hs_DINamespace_GetExportSymbols" getNamespaceExportedSymbols ::
  Ptr DINamespace -> IO LLVMBool

-- DIScope

foreign import ccall unsafe "LLVM_Hs_DIScope_GetScope" getScopeScope ::
  Ptr DIScope -> IO (Ptr DIScope)

foreign import ccall unsafe "LLVM_Hs_DIScope_GetFile" getScopeFile ::
  Ptr DIScope -> IO (Ptr DIFile)

-- DILexicalBlockBase
foreign import ccall unsafe "LLVM_Hs_DILexicalBlockBaseGetScope" getLexicalBlockScope ::
  Ptr DILexicalBlockBase -> IO (Ptr DILocalScope)

-- DILexicalBlockFile
foreign import ccall unsafe "LLVM_Hs_DILexicalBlockFileGetDiscriminator" getLexicalBlockFileDiscriminator ::
  Ptr DILexicalBlockBase -> IO Word32

foreign import ccall unsafe "LLVM_Hs_Get_DILexicalBlockFile" getDILexicalBlockFile ::
  Ptr Context -> Ptr DILocalScope -> Ptr DIFile -> Word32 -> IO (Ptr DILexicalBlockFile)

-- DILexicalBlock
foreign import ccall unsafe "LLVM_Hs_DILexicalBlockGetLine" getLexicalBlockLine ::
  Ptr DILexicalBlockBase -> IO Word32

foreign import ccall unsafe "LLVM_Hs_DILexicalBlockGetColumn" getLexicalBlockColumn ::
  Ptr DILexicalBlockBase -> IO Word16

foreign import ccall unsafe "LLVM_Hs_Get_DILexicalBlock" getDILexicalBlock ::
  Ptr Context -> Ptr DILocalScope -> Ptr DIFile -> Word32 -> Word16 -> IO (Ptr DILexicalBlock)

foreign import ccall unsafe "LLVM_Hs_DIDerivedTypeGetBaseType" getDerivedBaseType ::
  Ptr DIType -> IO (Ptr DIType)

foreign import ccall unsafe "LLVM_Hs_DIDerivedTypeGetAddressSpace" getDerivedAddressSpace ::
  Ptr DIType -> Ptr CUInt -> IO LLVMBool

-- DISubroutineType

foreign import ccall unsafe "LLVM_Hs_Get_DISubroutineType" getDISubroutineType ::
  Ptr Context -> DIFlags -> Word8 -> TupleArray DIType -> IO (Ptr DISubroutineType)

foreign import ccall unsafe "LLVM_Hs_DISubroutineType_GetCC" getSubroutineCC ::
  Ptr DISubroutineType -> IO Word8

foreign import ccall unsafe "LLVM_Hs_DISubroutine_GetTypeArray" getSubroutineTypeArray ::
  Ptr DISubroutineType -> IO (TupleArray DIType)

-- | DIBasicType

foreign import ccall unsafe "LLVM_Hs_Get_DIBasicType" getDIBasicType ::
  Ptr Context -> DwTag -> Ptr MDString -> Word64 -> Word32 -> Encoding -> DIFlags -> IO (Ptr DIBasicType)

foreign import ccall unsafe "LLVM_Hs_DIBasicType_GetEncoding" getBasicTypeEncoding ::
  Ptr DIBasicType -> IO Encoding

-- DIDerivedType

foreign import ccall unsafe "LLVM_Hs_Get_DIDerivedType" getDIDerivedType ::
  Ptr Context ->
  DwTag -> Ptr MDString -> Ptr DIFile -> CUInt -> Ptr DIScope ->
  Ptr DIType -> Word64 -> Word32 -> Word64 -> Word32 -> LLVMBool -> DIFlags ->
  IO (Ptr DIDerivedType)

foreign import ccall unsafe "LLVM_Hs_Get_DIFile" getDIFile ::
  Ptr Context -> Ptr MDString -> Ptr MDString -> ChecksumKind -> Ptr MDString -> IO (Ptr DIFile)

-- DISubrange
foreign import ccall unsafe "LLVM_Hs_Get_DISubrangeConstantCount" getDISubrangeConstantCount ::
  Ptr Context -> Int64 -> Int64 -> IO (Ptr DISubrange)

foreign import ccall unsafe "LLVM_Hs_Get_DISubrangeVariableCount" getDISubrangeVariableCount ::
  Ptr Context -> Ptr DIVariable -> Int64 -> IO (Ptr DISubrange)

foreign import ccall unsafe "LLVM_Hs_DISubrange_HasConstantCount" getDISubrangeHasConstantCount ::
  Ptr DISubrange -> IO LLVMBool

foreign import ccall unsafe "LLVM_Hs_DISubrange_GetVariableCount" getDISubrangeCountVariable ::
  Ptr DISubrange -> IO (Ptr DIVariable)

foreign import ccall unsafe "LLVM_Hs_DISubrange_GetConstantCount" getDISubrangeCountConstant ::
  Ptr DISubrange -> IO Int64

foreign import ccall unsafe "LLVM_Hs_DISubrange_GetLowerBound" getDISubrangeLowerBound ::
  Ptr DISubrange -> IO Int64

-- DISubprogram

foreign import ccall unsafe "LLVM_Hs_DISubprogram_GetLine" getDISubprogramLine ::
  Ptr DISubprogram -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_DISubprogram_GetVirtuality" getDISubprogramVirtuality ::
  Ptr DISubprogram -> IO DwVirtuality

foreign import ccall unsafe "LLVM_Hs_DISubprogram_GetVirtualIndex" getDISubprogramVirtualIndex ::
  Ptr DISubprogram -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_DISubprogram_GetScopeLine" getDISubprogramScopeLine ::
  Ptr DISubprogram -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_DISubprogram_IsOptimized" getDISubprogramIsOptimized ::
  Ptr DISubprogram -> IO LLVMBool

foreign import ccall unsafe "LLVM_Hs_DISubprogram_IsDefinition" getDISubprogramIsDefinition ::
  Ptr DISubprogram -> IO LLVMBool

foreign import ccall unsafe "LLVM_Hs_DISubprogram_GetLocalToUnit" getDISubprogramLocalToUnit ::
  Ptr DISubprogram -> IO LLVMBool

foreign import ccall unsafe "LLVM_Hs_DISubprogram_GetThisAdjustment" getDISubprogramThisAdjustment ::
  Ptr DISubprogram -> IO Int32

foreign import ccall unsafe "LLVM_Hs_DISubprogram_GetFlags" getDISubprogramFlags ::
  Ptr DISubprogram -> IO DIFlags

foreign import ccall unsafe "LLVM_Hs_DISubprogram_GetLinkageName" getDISubprogramLinkageName ::
  Ptr DISubprogram -> IO (Ptr MDString)

foreign import ccall unsafe "LLVM_Hs_DISubprogram_GetType" getDISubprogramType ::
  Ptr DISubprogram -> IO (Ptr DISubroutineType)

foreign import ccall unsafe "LLVM_Hs_DISubprogram_GetContainingType" getDISubprogramContainingType ::
  Ptr DISubprogram -> IO (Ptr DIType)

foreign import ccall unsafe "LLVM_Hs_DISubprogram_GetUnit" getDISubprogramUnit ::
  Ptr DISubprogram -> IO (Ptr DICompileUnit)

foreign import ccall unsafe "LLVM_Hs_DISubprogram_GetTemplateParams" getDISubprogramTemplateParams ::
  Ptr DISubprogram -> IO (TupleArray DITemplateParameter)

foreign import ccall unsafe "LLVM_Hs_DISubprogram_GetRetainedNodes" getDISubprogramRetainedNodes ::
  Ptr DISubprogram -> IO (TupleArray DILocalVariable)

foreign import ccall unsafe "LLVM_Hs_DISubprogram_GetThrownTypes" getDISubprogramThrownTypes ::
  Ptr DISubprogram -> IO (TupleArray DIType)

foreign import ccall unsafe "LLVM_Hs_DISubprogram_GetDeclaration" getDISubprogramDeclaration ::
  Ptr DISubprogram -> IO (Ptr DISubprogram)

foreign import ccall unsafe "LLVM_Hs_Get_DISubprogram" getDISubprogram ::
  Ptr Context -> Ptr DIScope -> Ptr MDString ->
  Ptr MDString -> Ptr DIFile -> CUInt ->
  Ptr DISubroutineType -> LLVMBool -> LLVMBool -> CUInt ->
  Ptr DIType -> DwVirtuality -> CUInt ->
  Int32 -> DIFlags -> LLVMBool ->
  Ptr DICompileUnit -> TupleArray DITemplateParameter -> Ptr DISubprogram ->
  TupleArray DILocalVariable -> TupleArray DIType ->
  IO (Ptr DISubprogram)

-- DIExpression

foreign import ccall unsafe "LLVM_Hs_Get_DIExpression" getDIExpression' ::
  Ptr Context -> CUInt -> Ptr Word64 -> IO (Ptr DIExpression)

getDIExpression :: Ptr Context -> (CUInt, Ptr Word64) -> IO (Ptr DIExpression)
getDIExpression ctx (numOps, ops) = getDIExpression' ctx numOps ops

foreign import ccall unsafe "LLVM_Hs_DIExpression_GetNumElements" getDIExpressionNumElements ::
  Ptr DIExpression -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_DIExpression_GetElement" getDIExpressionElement ::
  Ptr DIExpression -> CUInt -> IO Word64

-- DIVariable

foreign import ccall unsafe "LLVM_Hs_DIVariable_GetScope" getDIVariableScope ::
  Ptr DIVariable -> IO (Ptr DIScope)

foreign import ccall unsafe "LLVM_Hs_DIVariable_GetFile" getDIVariableFile ::
  Ptr DIVariable -> IO (Ptr DIFile)

foreign import ccall unsafe "LLVM_Hs_DIVariable_GetName" getDIVariableName ::
  Ptr DIVariable -> IO (Ptr MDString)

foreign import ccall unsafe "LLVM_Hs_DIVariable_GetLine" getDIVariableLine ::
  Ptr DIVariable -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_DIVariable_GetType" getDIVariableType ::
  Ptr DIVariable -> IO (Ptr DIType)

foreign import ccall unsafe "LLVM_Hs_DIVariable_GetAlignInBits" getDIVariableAlignInBits ::
  Ptr DIVariable -> IO Word32

-- DILocalVariable

foreign import ccall unsafe "LLVM_Hs_Get_DILocalVariable" getDILocalVariable ::
  Ptr Context ->
  Ptr DIScope -> CString -> Ptr DIFile -> Word32 -> Ptr DIType -> Word16 -> DIFlags -> Word32 ->
  IO (Ptr DILocalVariable)

foreign import ccall unsafe "LLVM_Hs_DILocalVariable_GetArg" getDILocalVariableArg ::
  Ptr DILocalVariable -> IO Word16

foreign import ccall unsafe "LLVM_Hs_DILocalVariable_GetFlags" getDILocalVariableFlags ::
  Ptr DILocalVariable -> IO DIFlags

-- DIGlobalVariable

foreign import ccall unsafe "LLVM_Hs_Get_DIGlobalVariable" getDIGlobalVariable ::
  Ptr Context ->
  Ptr DIScope -> Ptr MDString -> Ptr MDString ->
  Ptr DIFile -> CUInt -> Ptr DIType ->
  LLVMBool -> LLVMBool ->
  Ptr DIDerivedType ->
  TupleArray DITemplateParameter ->
  Word32 ->
  IO (Ptr DIGlobalVariable)

foreign import ccall unsafe "LLVM_Hs_DIGlobalVariable_GetLocal" getDIGlobalVariableLocal ::
  Ptr DIGlobalVariable -> IO LLVMBool

foreign import ccall unsafe "LLVM_Hs_DIGlobalVariable_GetDefinition" getDIGlobalVariableDefinition ::
  Ptr DIGlobalVariable -> IO LLVMBool

foreign import ccall unsafe "LLVM_Hs_DIGlobalVariable_GetLinkageName" getDIGlobalVariableLinkageName ::
  Ptr DIGlobalVariable -> IO (Ptr MDString)

foreign import ccall unsafe "LLVM_Hs_DIGlobalVariable_GetStaticDataMemberDeclaration" getDIGlobalVariableStaticDataMemberDeclaration ::
  Ptr DIGlobalVariable -> IO (Ptr DIDerivedType)

-- DICompileUnit

foreign import ccall unsafe "LLVM_Hs_Get_DICompileUnit" getDICompileUnit ::
  Ptr Context ->
  CUInt -> Ptr DIFile -> Ptr MDString -> LLVMBool -> Ptr MDString ->
  CUInt -> Ptr MDString -> DebugEmissionKind -> TupleArray DICompositeType -> TupleArray DIScope ->
  TupleArray DIGlobalVariableExpression -> TupleArray DIImportedEntity -> TupleArray DIMacroNode ->
  Word64 -> LLVMBool ->
  LLVMBool -> DebugNameTableKind -> LLVMBool ->
  IO (Ptr DICompileUnit)

foreign import ccall unsafe "LLVM_Hs_DICompileUnit_GetLanguage" getDICompileUnitLanguage ::
  Ptr DICompileUnit -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_DICompileUnit_GetSplitDebugInlining" getDICompileUnitSplitDebugInlining ::
  Ptr DICompileUnit -> IO LLVMBool

foreign import ccall unsafe "LLVM_Hs_DICompileUnit_GetDebugInfoForProfiling" getDICompileUnitDebugInfoForProfiling ::
  Ptr DICompileUnit -> IO LLVMBool

foreign import ccall unsafe "LLVM_Hs_DICompileUnit_GetOptimized" getDICompileUnitOptimized ::
  Ptr DICompileUnit -> IO LLVMBool

foreign import ccall unsafe "LLVM_Hs_DICompileUnit_GetRuntimeVersion" getDICompileUnitRuntimeVersion ::
  Ptr DICompileUnit -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_DICompileUnit_GetProducer" getDICompileUnitProducer ::
  Ptr DICompileUnit -> IO (Ptr MDString)

foreign import ccall unsafe "LLVM_Hs_DICompileUnit_GetFlags" getDICompileUnitFlags ::
  Ptr DICompileUnit -> IO (Ptr MDString)

foreign import ccall unsafe "LLVM_Hs_DICompileUnit_GetSplitDebugFilename" getDICompileUnitSplitDebugFilename ::
  Ptr DICompileUnit -> IO (Ptr MDString)

foreign import ccall unsafe "LLVM_Hs_DICompileUnit_GetEmissionKind" getDICompileUnitEmissionKind ::
  Ptr DICompileUnit -> IO DebugEmissionKind

foreign import ccall unsafe "LLVM_Hs_DICompileUnit_GetNameTableKind" getDICompileUnitNameTableKind ::
  Ptr DICompileUnit -> IO DebugNameTableKind

foreign import ccall unsafe "LLVM_Hs_DICompileUnit_GetDWOId" getDICompileUnitDWOId ::
  Ptr DICompileUnit -> IO Word64

foreign import ccall unsafe "LLVM_Hs_DICompileUnit_GetEnumTypes" getDICompileUnitEnumTypes ::
  Ptr DICompileUnit -> IO (TupleArray DICompositeType)

foreign import ccall unsafe "LLVM_Hs_DICompileUnit_GetRetainedTypes" getDICompileUnitRetainedTypes ::
  Ptr DICompileUnit -> IO (TupleArray DIScope)

foreign import ccall unsafe "LLVM_Hs_DICompileUnit_GetGlobalVariables" getDICompileUnitGlobalVariables ::
  Ptr DICompileUnit -> IO (TupleArray DIGlobalVariableExpression)

foreign import ccall unsafe "LLVM_Hs_DICompileUnit_GetImportedEntities" getDICompileUnitImportedEntities ::
  Ptr DICompileUnit -> IO (TupleArray DIImportedEntity)

foreign import ccall unsafe "LLVM_Hs_DICompileUnit_GetMacros" getDICompileUnitMacros ::
  Ptr DICompileUnit -> IO (TupleArray DIMacroNode)

foreign import ccall unsafe "LLVM_Hs_DICompileUnit_GetRangesBaseAddress" getDICompileUnitRangesBaseAddress ::
  Ptr DICompileUnit -> IO LLVMBool

-- DIFlags
foreign import ccall unsafe "LLVM_Hs_DIFlags_GetFlag" getDIFlag ::
  CString -> IO DIFlags

-- DITemplateParameter

foreign import ccall unsafe "LLVM_Hs_DITemplateParameter_GetName" getDITemplateParameterName ::
  Ptr DITemplateParameter -> IO (Ptr MDString)

foreign import ccall unsafe "LLVM_Hs_DITemplateParameter_GetType" getDITemplateParameterType ::
  Ptr DITemplateParameter -> IO (Ptr DIType)

-- DITemplateTypeParameter

foreign import ccall unsafe "LLVM_Hs_Get_DITemplateTypeParameter" getDITemplateTypeParameter ::
  Ptr Context -> Ptr MDString -> Ptr DIType -> IO (Ptr DITemplateTypeParameter)

-- DITemplateValueParameter

foreign import ccall unsafe "LLVM_Hs_Get_DITemplateValueParameter" getDITemplateValueParameter ::
  Ptr Context -> Ptr MDString -> Ptr DIType -> DwTag -> Ptr Metadata -> IO (Ptr DITemplateValueParameter)

foreign import ccall unsafe "LLVM_Hs_DITemplateValueParameter_GetValue" getDITemplateValueParameterValue ::
  Ptr DITemplateValueParameter -> IO (Ptr Metadata)

-- DIMacro
foreign import ccall unsafe "LLVM_Hs_Get_DIMacro" getDIMacro ::
  Ptr Context -> Macinfo -> Word32 -> Ptr MDString -> Ptr MDString -> IO (Ptr DIMacro)

foreign import ccall unsafe "LLVM_Hs_DIMacro_GetMacinfo" getDIMacroMacinfo ::
  Ptr DIMacro -> IO Macinfo

foreign import ccall unsafe "LLVM_Hs_DIMacro_GetLine" getDIMacroLine ::
  Ptr DIMacro -> IO Word32

foreign import ccall unsafe "LLVM_Hs_DIMacro_GetName" getDIMacroName ::
  Ptr DIMacro -> IO (Ptr MDString)

foreign import ccall unsafe "LLVM_Hs_DIMacro_GetValue" getDIMacroValue ::
  Ptr DIMacro -> IO (Ptr MDString)

-- DIMacroFile
foreign import ccall unsafe "LLVM_Hs_Get_DIMacroFile" getDIMacroFile ::
  Ptr Context -> Word32 -> Ptr DIFile -> TupleArray DIMacroNode -> IO (Ptr DIMacroFile)

foreign import ccall unsafe "LLVM_Hs_DIMacroFile_GetLine" getDIMacroFileLine ::
  Ptr DIMacroFile -> IO Word32

foreign import ccall unsafe "LLVM_Hs_DIMacroFile_GetFile" getDIMacroFileFile ::
  Ptr DIMacroFile -> IO (Ptr DIFile)

foreign import ccall unsafe "LLVM_Hs_DIMacroFile_GetNumElements" getDIMacroFileNumElements ::
  Ptr DIMacroFile -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_DIMacroFile_GetElement" getDIMacroFileElement ::
  Ptr DIMacroFile -> CUInt -> IO (Ptr DIMacroNode)

-- DIImportedEntity
foreign import ccall unsafe "LLVM_Hs_Get_DIImportedEntity" getDIImportedEntity ::
  Ptr Context -> DwTag -> Ptr DIScope -> Ptr DINode -> Ptr DIFile -> Word32 -> Ptr MDString -> IO (Ptr DIImportedEntity)

foreign import ccall unsafe "LLVM_Hs_DIImportedEntity_GetLine" getDIImportedEntityLine ::
  Ptr DIImportedEntity -> IO Word32

foreign import ccall unsafe "LLVM_Hs_DIImportedEntity_GetScope" getDIImportedEntityScope ::
  Ptr DIImportedEntity -> IO (Ptr DIScope)

foreign import ccall unsafe "LLVM_Hs_DIImportedEntity_GetEntity" getDIImportedEntityEntity ::
  Ptr DIImportedEntity -> IO (Ptr DINode)

foreign import ccall unsafe "LLVM_Hs_DIImportedEntity_GetName" getDIImportedEntityName ::
  Ptr DIImportedEntity -> IO (Ptr MDString)

foreign import ccall unsafe "LLVM_Hs_DIImportedEntity_GetFile" getDIImportedEntityFile ::
  Ptr DIImportedEntity -> IO (Ptr DIFile)

-- DIGlobalVariableExpression
foreign import ccall unsafe "LLVM_Hs_Get_DIGlobalVariableExpression" getDIGlobalVariableExpression ::
  Ptr Context -> Ptr DIGlobalVariable -> Ptr DIExpression -> IO (Ptr DIGlobalVariableExpression)

foreign import ccall unsafe "LLVM_Hs_DIGlobalVariableExpression_GetVariable" getDIGlobalVariableExpressionVariable ::
  Ptr DIGlobalVariableExpression -> IO (Ptr DIGlobalVariable)

foreign import ccall unsafe "LLVM_Hs_DIGlobalVariableExpression_GetExpression" getDIGlobalVariableExpressionExpression ::
  Ptr DIGlobalVariableExpression -> IO (Ptr DIExpression)

-- DIObjCProperty
foreign import ccall unsafe "LLVM_Hs_Get_DIObjCProperty" getDIObjCProperty ::
  Ptr Context -> Ptr MDString -> Ptr DIFile -> Word32 -> Ptr MDString -> Ptr MDString -> Word32 -> Ptr DIType -> IO (Ptr DIObjCProperty)

foreign import ccall unsafe "LLVM_Hs_DIObjCProperty_GetLine" getDIObjCPropertyLine ::
  Ptr DIObjCProperty -> IO Word32

foreign import ccall unsafe "LLVM_Hs_DIObjCProperty_GetAttributes" getDIObjCPropertyAttributes ::
  Ptr DIObjCProperty -> IO Word32

foreign import ccall unsafe "LLVM_Hs_DIObjCProperty_GetName" getDIObjCPropertyName ::
  Ptr DIObjCProperty -> IO (Ptr MDString)

foreign import ccall unsafe "LLVM_Hs_DIObjCProperty_GetFile" getDIObjCPropertyFile ::
  Ptr DIObjCProperty -> IO (Ptr DIFile)

foreign import ccall unsafe "LLVM_Hs_DIObjCProperty_GetGetterName" getDIObjCPropertyGetterName ::
  Ptr DIObjCProperty -> IO (Ptr MDString)

foreign import ccall unsafe "LLVM_Hs_DIObjCProperty_GetSetterName" getDIObjCPropertySetterName ::
  Ptr DIObjCProperty -> IO (Ptr MDString)

foreign import ccall unsafe "LLVM_Hs_DIObjCProperty_GetType" getDIObjCPropertyType ::
  Ptr DIObjCProperty -> IO (Ptr DIType)

-- DIModule
foreign import ccall unsafe "LLVM_Hs_Get_DIModule" getDIModule ::
  Ptr Context -> Ptr DIScope -> Ptr MDString -> Ptr MDString -> Ptr MDString -> Ptr MDString -> IO (Ptr DIModule)

foreign import ccall unsafe "LLVM_Hs_DIModule_GetConfigurationMacros" getDIModuleConfigurationMacros ::
  Ptr DIModule -> IO (Ptr MDString)

foreign import ccall unsafe "LLVM_Hs_DIModule_GetIncludePath" getDIModuleIncludePath ::
  Ptr DIModule -> IO (Ptr MDString)

foreign import ccall unsafe "LLVM_Hs_DIModule_GetISysRoot" getDIModuleISysRoot ::
  Ptr DIModule -> IO (Ptr MDString)
