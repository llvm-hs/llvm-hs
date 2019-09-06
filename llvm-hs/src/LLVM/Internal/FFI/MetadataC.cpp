#define __STDC_LIMIT_MACROS

#include <iostream>
#include "llvm/Support/FormattedStream.h"

#include "llvm/Config/llvm-config.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm-c/Core.h"

using namespace llvm;

extern "C" {

LLVMMetadataRef LLVM_Hs_IsAMDString(LLVMMetadataRef md) {
    if (isa<MDString>(unwrap(md))) {
        return md;
    }
    return nullptr;
}

const char *LLVM_Hs_GetMDStringValue(LLVMMetadataRef md, unsigned* len) {
    if (const MDString *S = dyn_cast<MDString>(unwrap(md))) {
      *len = S->getString().size();
      return S->getString().data();
    }
    *len = 0;
    return nullptr;
}

MDString* LLVM_Hs_GetMDString(LLVMContextRef c, const char* str, unsigned len) {
    if (len == 0) {
        return nullptr;
    }
    return MDString::get(*unwrap(c), StringRef(str, len));
}

LLVMMetadataRef LLVM_Hs_MDValue(LLVMValueRef v) {
    return wrap(ValueAsMetadata::get(unwrap(v)));
}

LLVMValueRef LLVM_Hs_MetadataOperand(LLVMContextRef c, LLVMMetadataRef md) {
    return wrap(MetadataAsValue::get(*unwrap(c), unwrap(md)));
}

LLVMMetadataRef LLVM_Hs_IsAMDNode(LLVMMetadataRef md) {
    if (isa<MDNode>(unwrap(md))) {
        return md;
    }
    return nullptr;
}

LLVMValueRef LLVM_Hs_GetMDValue(LLVMMetadataRef md) {
    return wrap(unwrap<ValueAsMetadata>(md)->getValue());
}

LLVMMetadataRef LLVM_Hs_GetMetadataOperand(LLVMValueRef val) {
    return wrap(unwrap<MetadataAsValue>(val)->getMetadata());
}

MDTuple* LLVM_Hs_Get_MDTuple(LLVMContextRef c,
                                    LLVMMetadataRef *mds,
                                    unsigned count) {
    return MDTuple::get(*unwrap(c), {unwrap(mds), count});
}

LLVMMetadataRef LLVM_Hs_IsAMDValue(LLVMMetadataRef md) {
    if (isa<ValueAsMetadata>(unwrap(md))) {
        return md;
    }
    return nullptr;
}


LLVMValueRef LLVM_Hs_IsAMetadataOperand(LLVMValueRef val) {
    if (isa<MetadataAsValue>(unwrap(val))) {
        return val;
    }
    return nullptr;
}


unsigned LLVM_Hs_GetMDKindNames(
	LLVMContextRef c,
	const char **s,
	unsigned *l,
	unsigned n
) {
	SmallVector<StringRef, 8> ns;
	unwrap(c)->getMDKindNames(ns);
	if (ns.size() <= n) {
		for(unsigned i=0; i < ns.size(); ++i) {
			s[i] = ns[i].data();
			l[i] = ns[i].size();
		}
	}
	return ns.size();
}

unsigned LLVM_Hs_MDNode_GetNumOperands(MDNode* v) {
	return v->getNumOperands();
}

Metadata* LLVM_Hs_MDNode_GetOperand(MDNode* v, unsigned i) {
	return v->getOperand(i).get();
}

void LLVM_Hs_NamedMetadataAddOperands(
	NamedMDNode *n,
	LLVMMetadataRef *ops,
	unsigned nOps
) {
	for(unsigned i = 0; i != nOps; ++i) n->addOperand(unwrap<MDNode>(ops[i]));
}

const char *LLVM_Hs_GetNamedMetadataName(
	NamedMDNode *n,
	unsigned *len
) {
	StringRef s = n->getName();
	*len = s.size();
	return s.data();
}

const char *LLVM_Hs_GetStringRef(
    StringRef* s,
    unsigned *len
) {
    *len = s->size();
    return s->data();
}

unsigned LLVM_Hs_GetNamedMetadataNumOperands(NamedMDNode *n) {
	return n->getNumOperands();
}

void LLVM_Hs_GetNamedMetadataOperands(NamedMDNode *n, LLVMMetadataRef *dest) {
	for(unsigned i = 0; i != n->getNumOperands(); ++i)
		dest[i] = wrap(n->getOperand(i));
}

LLVMMetadataRef LLVM_Hs_CreateTemporaryMDNodeInContext(LLVMContextRef c) {
	return wrap(MDNode::getTemporary(*unwrap(c), ArrayRef<Metadata *>()).release());
}

void LLVM_Hs_DestroyTemporaryMDNode(LLVMMetadataRef v) {
    MDNode::deleteTemporary(unwrap<MDNode>(v));
}

void LLVM_Hs_GetMDNodeOperands(LLVMMetadataRef md, LLVMMetadataRef *dest) {
    const auto *N = cast<MDNode>(unwrap(md));
    const unsigned numOperands = N->getNumOperands();
    for (unsigned i = 0; i < numOperands; i++)
        dest[i] = wrap(N->getOperand(i));
}

void LLVM_Hs_MetadataReplaceAllUsesWith(LLVMMetadataRef md, LLVMMetadataRef replacement) {
    auto *Node = unwrap<MDNode>(md);
    Node->replaceAllUsesWith(unwrap<Metadata>(replacement));
    MDNode::deleteTemporary(Node);
}

// DILocation

DILocation* LLVM_Hs_Get_DILocation(LLVMContextRef cxt, uint32_t line, uint16_t column, DILocalScope* scope) {
    LLVMContext& c = *unwrap(cxt);

    return DILocation::get(c, line, column, scope);
}

uint32_t LLVM_Hs_DILocation_GetLine(DILocation *md) {
    return md->getLine();
}

uint16_t LLVM_Hs_DILocation_GetColumn(DILocation *md) {
    return md->getColumn();
}

DILocalScope* LLVM_Hs_DILocation_GetScope(DILocation *md) {
    return md->getScope();
}

unsigned LLVM_Hs_GetMetadataClassId(LLVMMetadataRef md) {
    return (unwrap(md))->getMetadataID();
}

uint16_t LLVM_Hs_DINodeGetTag(DINode *md) {
    return md->getTag();
}

DINode::DIFlags LLVM_Hs_DITypeGetFlags(DIType *md) {
    return md->getFlags();
}

// DIEnumerator

DIEnumerator* LLVM_Hs_Get_DIEnumerator(LLVMContextRef cxt, int64_t value, LLVMBool isUnsigned, MDString* name) {
    LLVMContext& c = *unwrap(cxt);
    return DIEnumerator::get(c, value, isUnsigned, name);
}

int64_t LLVM_Hs_DIEnumerator_GetValue(DIEnumerator* md) {
    return md->getValue();
}

LLVMBool LLVM_Hs_DIEnumerator_GetIsUnsigned(DIEnumerator* md) {
    return md->isUnsigned();
}

MDString* LLVM_Hs_DIEnumerator_GetName(DIEnumerator* md) {
    return md->getRawName();
}

MDString* LLVM_Hs_DIFileGetFilename(DIFile *di) {
    return di->getRawFilename();
}

MDString* LLVM_Hs_DIFileGetDirectory(DIFile *di) {
    return di->getRawDirectory();
}

MDString* LLVM_Hs_DIFileGetChecksum(DIFile *di) {
    auto checksumInfo = di->getRawChecksum();
    if (checksumInfo.hasValue()) {
        return checksumInfo->Value;
    }
    return nullptr;
}

llvm::DIFile::ChecksumKind LLVM_Hs_DIFileGetChecksumKind(DIFile *di) {
    auto checksumInfo = di->getRawChecksum();
    if (checksumInfo.hasValue()) {
        return checksumInfo->Kind;
    }
    return static_cast<llvm::DIFile::ChecksumKind>(0);
}

// DIScope

DIScope* LLVM_Hs_DIScope_GetScope(DIScope *ds) {
    return cast_or_null<DIScope>(ds->getScope());
}

DIFile* LLVM_Hs_DIScope_GetFile(DIScope *ds) {
    return ds->getFile();
}

const char* LLVM_Hs_DIScope_GetName(DIScope *ds, unsigned *len) {
    StringRef s = ds->getName();
    *len = s.size();
    return s.data();
}

// DINamespace

DINamespace* LLVM_Hs_Get_DINamespace(LLVMContextRef ctx, DIScope* scope, MDString* name, LLVMBool exportSymbols) {
    return DINamespace::get(*unwrap(ctx), scope, name, exportSymbols);
}

LLVMBool LLVM_Hs_DINamespace_GetExportSymbols(DINamespace *ds) {
    return ds->getExportSymbols();
}

MDString* LLVM_Hs_DITypeGetName(DIType *ds) {
    return ds->getRawName();
}

uint64_t LLVM_Hs_DITypeGetSizeInBits(DIType *ds) {
    return ds->getSizeInBits();
}

uint64_t LLVM_Hs_DITypeGetOffsetInBits(DIType *ds) {
    return ds->getOffsetInBits();
}

uint32_t LLVM_Hs_DITypeGetAlignInBits(DIType *ds) {
    return ds->getAlignInBits();
}

uint32_t LLVM_Hs_DITypeGetLine(DIType *ds) {
    return ds->getLine();
}

// DIBasicType

DIBasicType* LLVM_Hs_Get_DIBasicType(LLVMContextRef ctx, uint16_t tag, MDString *name, uint64_t sizeInBits, uint32_t alignInBits, unsigned encoding, DINode::DIFlags flags) {
    return DIBasicType::get(*unwrap(ctx), tag, name, sizeInBits, alignInBits, encoding, flags);
}


unsigned LLVM_Hs_DIBasicType_GetEncoding(DIBasicType *ds) {
    return ds->getEncoding();
}

// DIDerivedType
DIDerivedType* LLVM_Hs_Get_DIDerivedType(LLVMContextRef ctx, uint16_t tag, MDString* name, DIFile *file, unsigned line, DIScope *scope, DIType *baseType, uint64_t sizeInBits, uint32_t alignInBits, uint64_t offsetInBits, uint32_t dwarfAddressSpace, LLVMBool dwarfAddressSpacePresent, DINode::DIFlags flags) {
    LLVMContext& c = *unwrap(ctx);
    Optional<unsigned> addrSpace;
    if (dwarfAddressSpacePresent) {
        addrSpace = dwarfAddressSpace;
    }
    return DIDerivedType::get(c, tag, name, file, line, scope, baseType, sizeInBits, alignInBits, offsetInBits, addrSpace, flags);
}

DIFile* LLVM_Hs_Get_DIFile(LLVMContextRef ctx, MDString* filename, MDString* directory, unsigned checksumKind, MDString* checksum) {
    LLVMContext& c = *unwrap(ctx);
    if (!checksum) {
        return DIFile::get(c, filename, directory);
    }
    return DIFile::get(c, filename, directory, DIFile::ChecksumInfo<MDString*>(static_cast<llvm::DIFile::ChecksumKind>(checksumKind), checksum));
}

DISubrange* LLVM_Hs_Get_DISubrangeConstantCount(LLVMContextRef ctx, int64_t count, int64_t lowerBound) {
    return DISubrange::get(*unwrap(ctx), count, lowerBound);
}

DISubrange* LLVM_Hs_Get_DISubrangeVariableCount(LLVMContextRef ctx, DIVariable* count, int64_t lowerBound) {
    return DISubrange::get(*unwrap(ctx), count, lowerBound);
}

LLVMBool LLVM_Hs_DISubrange_HasConstantCount(DISubrange* range) {
    return range->getCount().is<ConstantInt*>();
}

int64_t LLVM_Hs_DISubrange_GetConstantCount(DISubrange* range) {
    return range->getCount().dyn_cast<ConstantInt*>()->getSExtValue();
}

DIVariable* LLVM_Hs_DISubrange_GetVariableCount(DISubrange* range) {
    return range->getCount().dyn_cast<DIVariable*>();
}


int64_t LLVM_Hs_DISubrange_GetLowerBound(DISubrange* range) {
    return range->getLowerBound();
}

MDTuple* LLVM_Hs_DICompositeType_GetElements(DICompositeType *dt) {
    return cast_or_null<MDTuple>(dt->getRawElements());
}

DIType* LLVM_Hs_DICompositeTypeGetVTableHolder(DICompositeType *dt) {
    return dt->getVTableHolder();
}

DIType* LLVM_Hs_DICompositeTypeGetBaseType(DICompositeType *dt) {
    return dt->getBaseType();
}

DIType* LLVM_Hs_DIDerivedTypeGetBaseType(DIDerivedType *dt) {
    return dt->getBaseType();
}

uint16_t LLVM_Hs_DICompositeTypeGetRuntimeLang(DICompositeType *dt) {
    return dt->getRuntimeLang();
}

MDTuple* LLVM_Hs_DICompositeType_GetTemplateParameters(DICompositeType *dt) {
    return cast_or_null<MDTuple>(dt->getRawTemplateParams());
}

MDString* LLVM_Hs_DICompositeTypeGetIdentifier(DICompositeType *dt) {
    return dt->getRawIdentifier();
}

DICompositeType *LLVM_Hs_Get_DIArrayType(LLVMContextRef ctx, MDTuple* subscripts, DIType* elTy, uint64_t size, uint32_t align, DINode::DIFlags flags) {
    return DICompositeType::get(*unwrap(ctx), dwarf::DW_TAG_array_type, "", nullptr, 0, nullptr, elTy, size, align, 0, flags, subscripts, 0, nullptr);
}

DICompositeType* LLVM_Hs_Get_DIEnumerationType(LLVMContextRef ctx, DIScope* scope, MDString* name, DIFile *file, uint32_t line, uint64_t size, uint32_t align, MDTuple* elements, DIType* underlyingType, MDString* uniqueIdentifier) {
    return DICompositeType::get(*unwrap(ctx), dwarf::DW_TAG_enumeration_type, name, file, line, scope, underlyingType, size, align, 0, DINode::FlagZero, elements, 0, nullptr, nullptr, uniqueIdentifier);
}

DICompositeType *LLVM_Hs_Get_DIStructType(
    LLVMContextRef ctx, DIScope *scope, MDString *name, DIFile *file,
    uint32_t line, uint64_t size, uint32_t align, DINode::DIFlags flags,
    DIType *derivedFrom, MDTuple *elements,
    uint16_t runtimeLang, DIType *vtableHolder, MDString *uniqueIdentifier) {
    return DICompositeType::get(*unwrap(ctx), dwarf::DW_TAG_structure_type, name, file,
                                line, scope, derivedFrom, size, align, 0, flags,
                                elements, runtimeLang, vtableHolder, nullptr,
                                uniqueIdentifier);
}

DICompositeType *LLVM_Hs_Get_DIUnionType(
    LLVMContextRef ctx, DIScope *scope, MDString *name, DIFile *file,
    uint32_t line, uint64_t size, uint32_t align, DINode::DIFlags flags,
    MDTuple *elements,
    uint16_t runtimeLang, MDString *uniqueIdentifier) {
    return DICompositeType::get(*unwrap(ctx), dwarf::DW_TAG_union_type, name, file,
                                line, scope, nullptr, size, align, 0, flags,
                                elements, runtimeLang, nullptr, nullptr,
                                uniqueIdentifier);
}

DICompositeType *LLVM_Hs_Get_DIClassType(
    LLVMContextRef ctx, DIScope *scope, MDString *name, DIFile *file,
    uint32_t line, uint64_t size, uint32_t align, DINode::DIFlags flags,
    DIType *derivedFrom, MDTuple *elements,
    DIType *vtableHolder, MDTuple* templateParams, MDString *uniqueIdentifier) {
    return DICompositeType::get(*unwrap(ctx), dwarf::DW_TAG_class_type, name, file,
                                line, scope, derivedFrom, size, align, 0, flags,
                                elements, 0, vtableHolder, templateParams,
                                uniqueIdentifier);
}


// DILexicalBlockBase

DILocalScope* LLVM_Hs_DILexicalBlockBaseGetScope(DILexicalBlockBase* bb) {
    return bb->getScope();
}

// DILexicalBlockFile

uint32_t LLVM_Hs_DILexicalBlockFileGetDiscriminator(DILexicalBlockFile* bf) {
    return bf->getDiscriminator();
}

DILexicalBlockFile* LLVM_Hs_Get_DILexicalBlockFile(LLVMContextRef ctx, DILocalScope* scope, DIFile* file, uint32_t discriminator) {
    return DILexicalBlockFile::get(*unwrap(ctx), scope, file, discriminator);
}

// DILexicalBlock

uint32_t LLVM_Hs_DILexicalBlockGetLine(DILexicalBlock* lb) {
    return lb->getLine();
}

uint16_t LLVM_Hs_DILexicalBlockGetColumn(DILexicalBlock* lb) {
    return lb->getColumn();
}

DILexicalBlock* LLVM_Hs_Get_DILexicalBlock(LLVMContextRef ctx, DILocalScope* scope, DIFile *file, uint32_t line, uint16_t column) {
    return DILexicalBlock::get(*unwrap(ctx), scope, file, line, column);
}

LLVMBool LLVM_Hs_DIDerivedTypeGetAddressSpace(DIDerivedType *a, unsigned *x) {
    auto addressSpace = a->getDWARFAddressSpace();
    if (addressSpace.hasValue()) {
        *x = addressSpace.getValue();
        return 1;
    } else {
        return 0;
    }
}

// DISubroutineType

DISubroutineType* LLVM_Hs_Get_DISubroutineType(LLVMContextRef ctx, DINode::DIFlags flags, uint8_t cc, MDTuple* types) {
    return DISubroutineType::get(*unwrap(ctx), flags, cc, types);
}

uint8_t LLVM_Hs_DISubroutineType_GetCC(DISubroutineType *a) {
    return a->getCC();
}

MDTuple* LLVM_Hs_DISubroutine_GetTypeArray(DISubroutineType *md) {
    return cast_or_null<MDTuple>(md->getRawTypeArray());
}

// DISubprogram

unsigned LLVM_Hs_DISubprogram_GetLine(DISubprogram* p) {
    return p->getLine();
}

uint8_t LLVM_Hs_DISubprogram_GetVirtuality(DISubprogram* p) {
    return p->getVirtuality();
}

unsigned LLVM_Hs_DISubprogram_GetVirtualIndex(DISubprogram* p) {
    return p->getVirtualIndex();
}

unsigned LLVM_Hs_DISubprogram_GetScopeLine(DISubprogram* p) {
    return p->getScopeLine();
}

LLVMBool LLVM_Hs_DISubprogram_IsOptimized(DISubprogram* p) {
    return p->isOptimized();
}

LLVMBool LLVM_Hs_DISubprogram_IsDefinition(DISubprogram* p) {
    return p->isDefinition();
}

LLVMBool LLVM_Hs_DISubprogram_GetLocalToUnit(DISubprogram* p) {
    return p->isLocalToUnit();
}

int32_t LLVM_Hs_DISubprogram_GetThisAdjustment(DISubprogram* p) {
    return p->getThisAdjustment();
}

DINode::DIFlags LLVM_Hs_DISubprogram_GetFlags(DISubprogram* p) {
    return p->getFlags();
}

MDString* LLVM_Hs_DISubprogram_GetLinkageName(DISubprogram* p) {
    return p->getRawLinkageName();
}

DISubroutineType* LLVM_Hs_DISubprogram_GetType(DISubprogram* p) {
    return p->getType();
}

DIType* LLVM_Hs_DISubprogram_GetContainingType(DISubprogram* p) {
    return p->getContainingType();
}

DICompileUnit* LLVM_Hs_DISubprogram_GetUnit(DISubprogram* p) {
    return p->getUnit();
}

MDTuple* LLVM_Hs_DISubprogram_GetTemplateParams(DISubprogram* p) {
    return cast_or_null<MDTuple>(p->getRawTemplateParams());
}

MDTuple* LLVM_Hs_DISubprogram_GetRetainedNodes(DISubprogram* p) {
    return cast_or_null<MDTuple>(p->getRawRetainedNodes());
}

MDTuple* LLVM_Hs_DISubprogram_GetThrownTypes(DISubprogram* p) {
    return cast_or_null<MDTuple>(p->getRawThrownTypes());
}

DISubprogram* LLVM_Hs_DISubprogram_GetDeclaration(DISubprogram* p) {
    return p->getDeclaration();
}

DISubprogram *LLVM_Hs_Get_DISubprogram(
    LLVMContextRef ctx, DIScope *scope, MDString *name,
    MDString *linkageName, DIFile *file, unsigned line,
    DISubroutineType *type, LLVMBool isLocal, LLVMBool isDefinition, unsigned scopeLine,
    DIType *containingType, uint8_t virtuality, unsigned virtualIndex,
    int32_t thisAdjustment, DINode::DIFlags flags, LLVMBool isOptimized,
    DICompileUnit *unit, Metadata *templateParams, DISubprogram *declaration,
    Metadata *variables, Metadata *thrownTypes) {
    LLVMContext &c = *unwrap(ctx);
    DISubprogram::DISPFlags spFlags = DISubprogram::toSPFlags(isLocal, isDefinition, isOptimized, virtuality);
    if (isDefinition) {
        return DISubprogram::getDistinct(
          c, scope, name, linkageName, file,
          line, type, scopeLine, containingType,
          virtualIndex, thisAdjustment, flags, spFlags, unit,
          templateParams, declaration, variables, thrownTypes);
    } else {
        return DISubprogram::get(
          c, scope, name, linkageName, file,
          line, type, scopeLine, containingType,
          virtualIndex, thisAdjustment, flags, spFlags, unit,
          templateParams, declaration, variables, thrownTypes);
    }
}

// DIExpression

DIExpression* LLVM_Hs_Get_DIExpression(LLVMContextRef ctx, unsigned numOps, uint64_t* ops) {
    return DIExpression::get(*unwrap(ctx), {ops, numOps});
}

unsigned LLVM_Hs_DIExpression_GetNumElements(DIExpression* e) {
    return e->getNumElements();
}

uint64_t LLVM_Hs_DIExpression_GetElement(DIExpression* e, unsigned i) {
    return e->getElement(i);
}

// DIVariable

DIScope* LLVM_Hs_DIVariable_GetScope(DIVariable* v) {
    return v->getScope();
}

DIFile* LLVM_Hs_DIVariable_GetFile(DIVariable* v) {
    return v->getFile();
}

MDString* LLVM_Hs_DIVariable_GetName(DIVariable* v) {
    return v->getRawName();
}

unsigned LLVM_Hs_DIVariable_GetLine(DIVariable* v) {
    return v->getLine();
}

DIType* LLVM_Hs_DIVariable_GetType(DIVariable* v) {
    return v->getType();
}

uint32_t LLVM_Hs_DIVariable_GetAlignInBits(DIVariable* v) {
    return v->getAlignInBits();
}

// DILocalVariable

DILocalVariable* LLVM_Hs_Get_DILocalVariable(LLVMContextRef ctx,
                                             DIScope* scope, MDString* name, DIFile* file,
                                             uint32_t line, DIType* type, uint16_t arg,
                                             DINode::DIFlags flags, uint32_t alignInBits) {
    LLVMContext &c = *unwrap(ctx);
    return DILocalVariable::get(c, static_cast<DILocalScope*>(scope), name, file, line, type,
                                arg, flags, alignInBits);
}

uint16_t LLVM_Hs_DILocalVariable_GetArg(DILocalVariable* v) {
    return v->getArg();
}

DINode::DIFlags LLVM_Hs_DILocalVariable_GetFlags(DILocalVariable* v) {
    return v->getFlags();
}

// DIGlobalVariable

DIGlobalVariable* LLVM_Hs_Get_DIGlobalVariable(LLVMContextRef ctx,
                                               DIScope* scope, MDString* name, MDString* linkageName,
                                               DIFile* file, unsigned line, DIType* type,
                                               LLVMBool isLocalToUnit, LLVMBool isDefinition,
                                               DIDerivedType* declaration,
                                               Metadata* templateParams,
                                               uint32_t alignInBits) {
    LLVMContext &c = *unwrap(ctx);
    return DIGlobalVariable::get(c, scope, name, linkageName,
                                 file, line, type,
                                 isLocalToUnit, isDefinition,
                                 declaration, templateParams,
                                 alignInBits);
}

LLVMBool LLVM_Hs_DIGlobalVariable_GetLocal(DIGlobalVariable* v) {
    return v->isLocalToUnit();
}

LLVMBool LLVM_Hs_DIGlobalVariable_GetDefinition(DIGlobalVariable* v) {
    return v->isDefinition();
}

MDString* LLVM_Hs_DIGlobalVariable_GetLinkageName(DIGlobalVariable* v) {
    return v->getRawLinkageName();
}

DIDerivedType* LLVM_Hs_DIGlobalVariable_GetStaticDataMemberDeclaration(DIGlobalVariable* v) {
    return v->getStaticDataMemberDeclaration();
}

// DICompileUnit
DICompileUnit* LLVM_Hs_Get_DICompileUnit
  (LLVMContextRef ctx,
   unsigned sourceLanguage, DIFile* file, MDString* producer, LLVMBool isOptimized, MDString* flags,
   unsigned runtimeVersion, MDString* splitDebugFilename, unsigned emissionKind, Metadata* enumTypes, Metadata* retainedTypes,
   Metadata* globalVariables, Metadata* importedEntities, Metadata* macros, uint64_t dwoid, LLVMBool splitDebugInlining,
   LLVMBool debugInfoForProfiling, unsigned nameTableKind, LLVMBool debugBaseAddress) {
    LLVMContext &c = *unwrap(ctx);
    return DICompileUnit::getDistinct
        (c,
         sourceLanguage, file, producer, isOptimized, flags,
         runtimeVersion, splitDebugFilename, emissionKind, enumTypes, retainedTypes,
         globalVariables, importedEntities, macros, dwoid, splitDebugInlining,
         debugInfoForProfiling, nameTableKind, debugBaseAddress);
}

unsigned LLVM_Hs_DICompileUnit_GetLanguage(DICompileUnit* cu) {
    return cu->getSourceLanguage();
}

LLVMBool LLVM_Hs_DICompileUnit_GetSplitDebugInlining(DICompileUnit* cu) {
    return cu->getSplitDebugInlining();
}

LLVMBool LLVM_Hs_DICompileUnit_GetDebugInfoForProfiling(DICompileUnit* cu) {
    return cu->getDebugInfoForProfiling();
}

LLVMBool LLVM_Hs_DICompileUnit_GetOptimized(DICompileUnit* cu) {
    return cu->isOptimized();
}

unsigned LLVM_Hs_DICompileUnit_GetRuntimeVersion(DICompileUnit* cu) {
    return cu->getRuntimeVersion();
}

MDString* LLVM_Hs_DICompileUnit_GetProducer(DICompileUnit* cu) {
    return cu->getRawProducer();
}

MDString* LLVM_Hs_DICompileUnit_GetFlags(DICompileUnit* cu) {
    return cu->getRawFlags();
}

MDString* LLVM_Hs_DICompileUnit_GetSplitDebugFilename(DICompileUnit* cu) {
    return cu->getRawSplitDebugFilename();
}

unsigned LLVM_Hs_DICompileUnit_GetEmissionKind(DICompileUnit* cu) {
    return cu->getEmissionKind();
}

unsigned LLVM_Hs_DICompileUnit_GetNameTableKind(DICompileUnit* cu) {
    return static_cast<unsigned>(cu->getNameTableKind());
}

uint64_t LLVM_Hs_DICompileUnit_GetDWOId(DICompileUnit* cu) {
    return cu->getDWOId();
}

MDTuple* LLVM_Hs_DICompileUnit_GetEnumTypes(DICompileUnit* cu) {
    return cast_or_null<MDTuple>(cu->getRawEnumTypes());
}

MDTuple* LLVM_Hs_DICompileUnit_GetRetainedTypes(DICompileUnit* cu) {
    return cast_or_null<MDTuple>(cu->getRawRetainedTypes());
}

MDTuple* LLVM_Hs_DICompileUnit_GetGlobalVariables(DICompileUnit* cu) {
    return cast_or_null<MDTuple>(cu->getRawGlobalVariables());
}

MDTuple* LLVM_Hs_DICompileUnit_GetImportedEntities(DICompileUnit* cu) {
    return cast_or_null<MDTuple>(cu->getRawImportedEntities());
}

MDTuple* LLVM_Hs_DICompileUnit_GetMacros(DICompileUnit* cu) {
    return cast_or_null<MDTuple>(cu->getRawMacros());
}

LLVMBool LLVM_Hs_DICompileUnit_GetRangesBaseAddress(DICompileUnit* cu) {
    return cu->getRangesBaseAddress();
}

// DIFlags

// This is mainly intended for testing purposes
DINode::DIFlags LLVM_Hs_DIFlags_GetFlag(const char* flag) {
    return DINode::getFlag(flag);
}

// DITemplateParameter

MDString* LLVM_Hs_DITemplateParameter_GetName(DITemplateParameter* p) {
    return p->getRawName();
}

DIType* LLVM_Hs_DITemplateParameter_GetType(DITemplateParameter* p) {
    return p->getType();
}

// DITemplateTypeParameter

DITemplateTypeParameter* LLVM_Hs_Get_DITemplateTypeParameter(LLVMContextRef ctx, MDString* name, DIType* type) {
    return DITemplateTypeParameter::get(*unwrap(ctx), name, type);
}

// DITemplateValueParameter

DITemplateValueParameter* LLVM_Hs_Get_DITemplateValueParameter(LLVMContextRef ctx, MDString* name, DIType* type, uint16_t tag, Metadata* value) {
    return DITemplateValueParameter::get(*unwrap(ctx), tag, name, type, value);
}

Metadata* LLVM_Hs_DITemplateValueParameter_GetValue(DITemplateValueParameter* p) {
    return p->getValue();
}

// DIMacro
DIMacro* LLVM_Hs_Get_DIMacro(LLVMContextRef ctx, unsigned macinfo, uint32_t line, MDString* name, MDString* value) {
    return DIMacro::get(*unwrap(ctx), macinfo, line, name, value);
}

unsigned LLVM_Hs_DIMacro_GetMacinfo(DIMacro* m) {
    return m->getMacinfoType();
}

uint32_t LLVM_Hs_DIMacro_GetLine(DIMacro* m) {
    return m->getLine();
}

MDString* LLVM_Hs_DIMacro_GetName(DIMacro* m) {
    return m->getRawName();
}

MDString* LLVM_Hs_DIMacro_GetValue(DIMacro* m) {
    return m->getRawValue();
}

// DIMacroFile
DIMacroFile* LLVM_Hs_Get_DIMacroFile(LLVMContextRef ctx, uint32_t line, DIFile* file, MDTuple* elems) {
    return DIMacroFile::get(*unwrap(ctx), dwarf::DW_MACINFO_start_file, line, file, static_cast<Metadata*>(elems));
}

uint32_t LLVM_Hs_DIMacroFile_GetLine(DIMacroFile* m) {
    return m->getLine();
}

DIFile* LLVM_Hs_DIMacroFile_GetFile(DIMacroFile* m) {
    return m->getFile();
}

unsigned LLVM_Hs_DIMacroFile_GetNumElements(DIMacroFile* m) {
    return m->getElements().size();
}

DIMacroNode* LLVM_Hs_DIMacroFile_GetElement(DIMacroFile* m, unsigned i) {
    return m->getElements()[i];
}

// DIImportedEntity

DIImportedEntity* LLVM_Hs_Get_DIImportedEntity(LLVMContextRef ctx, uint16_t tag, DIScope* scope, DINode* entity, DIFile* file, uint32_t line, MDString* name) {
    return DIImportedEntity::get(*unwrap(ctx), tag, scope, entity, file, line, name);
}

uint32_t LLVM_Hs_DIImportedEntity_GetLine(DIImportedEntity* e) {
    return e->getLine();
}

DIScope* LLVM_Hs_DIImportedEntity_GetScope(DIImportedEntity* e) {
    return e->getScope();
}

DINode* LLVM_Hs_DIImportedEntity_GetEntity(DIImportedEntity* e) {
    return e->getEntity();
}

MDString* LLVM_Hs_DIImportedEntity_GetName(DIImportedEntity* e) {
    return e->getRawName();
}

DIFile* LLVM_Hs_DIImportedEntity_GetFile(DIImportedEntity* e) {
    return e->getFile();
}

// DIGlobalVariableExpression

DIGlobalVariableExpression* LLVM_Hs_Get_DIGlobalVariableExpression(LLVMContextRef ctx, DIGlobalVariable* var, DIExpression* expr) {
    return DIGlobalVariableExpression::get(*unwrap(ctx), var, expr);
}

DIGlobalVariable* LLVM_Hs_DIGlobalVariableExpression_GetVariable(DIGlobalVariableExpression* e) {
    return e->getVariable();
}

DIExpression* LLVM_Hs_DIGlobalVariableExpression_GetExpression(DIGlobalVariableExpression* e) {
    return e->getExpression();
}

// DIObjCProperty

DIObjCProperty* LLVM_Hs_Get_DIObjCProperty(LLVMContextRef ctx, MDString* name, DIFile* file, uint32_t line, MDString* getterName, MDString* setterName, uint32_t attributes, DIType* type) {
    return DIObjCProperty::get(*unwrap(ctx), name, file, line, getterName, setterName, attributes, type);
}

uint32_t LLVM_Hs_DIObjCProperty_GetLine(DIObjCProperty* o) {
    return o->getLine();
}

uint32_t LLVM_Hs_DIObjCProperty_GetAttributes(DIObjCProperty* o) {
    return o->getAttributes();
}

MDString* LLVM_Hs_DIObjCProperty_GetName(DIObjCProperty* o) {
    return o->getRawName();
}

DIFile* LLVM_Hs_DIObjCProperty_GetFile(DIObjCProperty* o) {
    return o->getFile();
}

MDString* LLVM_Hs_DIObjCProperty_GetGetterName(DIObjCProperty* o) {
    return o->getRawGetterName();
}

MDString* LLVM_Hs_DIObjCProperty_GetSetterName(DIObjCProperty* o) {
    return o->getRawSetterName();
}

DIType* LLVM_Hs_DIObjCProperty_GetType(DIObjCProperty* o) {
    return o->getType();
}

// DIModule

DIModule* LLVM_Hs_Get_DIModule(LLVMContextRef ctx, DIScope* scope, MDString* name, MDString* configurationMacros, MDString* includePath, MDString* isysRoot) {
    return DIModule::get(*unwrap(ctx), scope, name, configurationMacros, includePath, isysRoot);
}

MDString* LLVM_Hs_DIModule_GetConfigurationMacros(DIModule* m) {
    return m->getRawConfigurationMacros();
}

MDString* LLVM_Hs_DIModule_GetIncludePath(DIModule* m) {
    return m->getRawIncludePath();
}

MDString* LLVM_Hs_DIModule_GetISysRoot(DIModule* m) {
    return m->getRawISysRoot();
}
}
