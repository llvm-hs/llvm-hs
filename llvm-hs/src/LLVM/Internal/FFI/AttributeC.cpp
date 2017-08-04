#define __STDC_LIMIT_MACROS
#include "LLVM/Internal/FFI/AttributeC.hpp"
#include "llvm/IR/LLVMContext.h"

extern "C" {

static_assert(sizeof(AttributeList) == sizeof(AttributeListImpl *),
              "AttributeList implementation has changed");

static_assert(sizeof(Attribute) == sizeof(AttributeImpl *),
              "Attribute implementation has changed");

#define CHECK(name, p, r, f)                                                   \
    static_assert(unsigned(llvm::Attribute::name) ==                           \
                      unsigned(LLVM_Hs_AttributeKind_##name),                  \
                  "LLVM_Hs_AttributeKind enum out of sync w/ "                 \
                  "llvm::Attribute::AttrKind for " #name);
LLVM_HS_FOR_EACH_ATTRIBUTE_KIND(CHECK)
#undef CHECK

unsigned LLVM_Hs_AttributeKindAsEnum(LLVMAttributeRef a) {
    return unwrap(a).getKindAsEnum();
}

uint64_t LLVM_Hs_AttributeValueAsInt(LLVMAttributeRef a) {
    return unwrap(a).getValueAsInt();
}

LLVMBool LLVM_Hs_IsStringAttribute(LLVMAttributeRef a) {
    return unwrap(a).isStringAttribute();
}

const char *LLVM_Hs_AttributeKindAsString(LLVMAttributeRef a, size_t &l) {
    const StringRef s = unwrap(a).getKindAsString();
    l = s.size();
    return s.data();
}

const char *LLVM_Hs_AttributeValueAsString(LLVMAttributeRef a, size_t &l) {
    const StringRef s = unwrap(a).getValueAsString();
    l = s.size();
    return s.data();
}

LLVMAttributeListRef LLVM_Hs_GetAttributeList(LLVMContextRef context,
                                              unsigned index,
                                              LLVMAttributeSetRef as) {
    return new AttributeList(AttributeList::get(*unwrap(context), index, *as));
}

LLVMAttributeListRef LLVM_Hs_BuildAttributeList(LLVMContextRef context,
                                                LLVMAttributeSetRef fAttrs,
                                                LLVMAttributeSetRef rAttrs,
                                                LLVMAttributeSetRef *pAttrs,
                                                unsigned numPAttrs) {
    std::vector<AttributeSet> pAttrSets{numPAttrs};
    for (unsigned i = 0; i < numPAttrs; ++i) {
        pAttrSets[i] = *pAttrs[i];
    }
    return new AttributeList(
        AttributeList::get(*unwrap(context), *fAttrs, *rAttrs, pAttrSets));
}

void LLVM_Hs_DisposeAttributeList(LLVMAttributeListRef attributeList) {
    delete attributeList;
}

LLVMAttributeSetRef LLVM_Hs_GetAttributeSet(LLVMContextRef context,
                                            const AttrBuilder &ab) {
    return new AttributeSet(AttributeSet::get(*unwrap(context), ab));
}

void LLVM_Hs_DisposeAttributeSet(LLVMAttributeListRef attributeList) {
    delete attributeList;
}

LLVMBool LLVM_Hs_AttributeSetsEqual(LLVMAttributeSetRef as1,
                                    LLVMAttributeSetRef as2) {
    return *as1 == *as2;
}

LLVMBool LLVM_Hs_AttributeSetHasAttributes(LLVMAttributeSetRef as) {
    return as->hasAttributes();
}

unsigned LLVM_Hs_getNumAttributes(LLVMAttributeSetRef attributeSet) {
    return attributeSet->getNumAttributes();
}

void LLVM_Hs_getAttributes(LLVMAttributeSetRef attributeSet,
                           LLVMAttributeRef *attrs) {
    for (auto a : *attributeSet) {
        *attrs++ = wrap(a);
    }
}

size_t LLVM_Hs_GetAttrBuilderSize() { return sizeof(AttrBuilder); }

AttrBuilder *LLVM_Hs_ConstructAttrBuilder(char *p) {
    return new (p) AttrBuilder();
}

AttrBuilder *LLVM_Hs_AttrBuilderFromAttrSet(LLVMAttributeSetRef as) {
    return new AttrBuilder(*as);
}

void LLVM_Hs_DisposeAttrBuilder(LLVMAttributeSetRef as) { delete as; }

void LLVM_Hs_AttrBuilderMerge(AttrBuilder *ab1, AttrBuilder *ab2) {
    ab1->merge(*ab2);
}

void LLVM_Hs_DestroyAttrBuilder(AttrBuilder *a) { a->~AttrBuilder(); }

void LLVM_Hs_AttrBuilderAddAttributeKind(AttrBuilder &ab, unsigned kind) {
    ab.addAttribute(Attribute::AttrKind(kind));
}

void LLVM_Hs_AttrBuilderAddStringAttribute(AttrBuilder &ab, const char *kind,
                                           size_t kind_len, const char *value,
                                           size_t value_len) {
    ab.addAttribute(StringRef(kind, kind_len), StringRef(value, value_len));
}

void LLVM_Hs_AttrBuilderAddAlignment(AttrBuilder &ab, uint64_t v) {
    ab.addAlignmentAttr(v);
}

void LLVM_Hs_AttrBuilderAddStackAlignment(AttrBuilder &ab, uint64_t v) {
    ab.addStackAlignmentAttr(v);
}

void LLVM_Hs_AttrBuilderAddAllocSize(AttrBuilder &ab, unsigned x, unsigned y,
                                     LLVMBool optionalIsThere) {
    if (optionalIsThere) {
        ab.addAllocSizeAttr(x, Optional<unsigned>(y));
    } else {
        ab.addAllocSizeAttr(x, Optional<unsigned>());
    }
}

void LLVM_Hs_AttrBuilderAddDereferenceableAttr(AttrBuilder &ab, uint64_t v) {
    ab.addDereferenceableAttr(v);
}

void LLVM_Hs_AttrBuilderAddDereferenceableOrNullAttr(AttrBuilder &ab,
                                                     uint64_t v) {
    ab.addDereferenceableOrNullAttr(v);
}

LLVMBool LLVM_Hs_AttributeGetAllocSizeArgs(LLVMAttributeRef a, unsigned *x,
                                           unsigned *y) {
    auto pair = unwrap(a).getAllocSizeArgs();
    *x = pair.first;
    if (pair.second.hasValue()) {
        *y = pair.second.getValue();
        return 1;
    } else {
        return 0;
    }
}
}
