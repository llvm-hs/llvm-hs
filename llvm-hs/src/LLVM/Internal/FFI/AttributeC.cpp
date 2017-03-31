#define __STDC_LIMIT_MACROS
#include "llvm/IR/LLVMContext.h"
#include "LLVM/Internal/FFI/AttributeC.hpp"

extern "C" {

static_assert(sizeof(AttributeSet) == sizeof(AttributeSetImpl *),
              "AttributeSet implementation has changed");

static_assert(sizeof(Attribute) == sizeof(AttributeImpl *),
              "Attribute implementation has changed");

unsigned LLVM_Hs_AttributeSetNumSlots(const AttributeSetImpl *a) {
	return unwrap(a).getNumSlots();
}

int LLVM_Hs_AttributeSetSlotIndex(const AttributeSetImpl *a, unsigned slot) {
	return unwrap(a).getSlotIndex(slot);
}

const AttributeSetImpl *LLVM_Hs_AttributeSetSlotAttributes(const AttributeSetImpl *a, unsigned slot) {
	return wrap(unwrap(a).getSlotAttributes(slot));
}

const AttributeSetImpl *LLVM_Hs_GetSlotAttributeSet(
	LLVMContextRef context,
	unsigned index,
	const AttributeImpl **attributes,
	unsigned length
) {
	AttrBuilder builder;
	for (unsigned i = 0; i < length; i++) builder.addAttribute(unwrap(attributes[i]));
	return wrap(AttributeSet::get(*unwrap(context), index, builder));
}

const AttributeImpl *const *LLVM_Hs_AttributeSetGetAttributes(AttributeSetImpl *asi, unsigned slot, unsigned *length) {
	AttributeSet as = unwrap(asi);
	ArrayRef<Attribute>::iterator b = as.begin(slot), e = as.end(slot);
	*length = e - b;
	return reinterpret_cast<const AttributeImpl *const *>(b);
}

#define CHECK(name,p,r,f)																								\
	static_assert(																												\
		unsigned(llvm::Attribute::name) == unsigned(LLVM_Hs_AttributeKind_ ## name), \
		"LLVM_Hs_AttributeKind enum out of sync w/ llvm::Attribute::AttrKind for " #name	\
	);
	LLVM_HS_FOR_EACH_ATTRIBUTE_KIND(CHECK)
#undef CHECK

unsigned LLVM_Hs_AttributeKindAsEnum(const AttributeImpl *a) {
    return unwrap(a).getKindAsEnum();
}

uint64_t LLVM_Hs_AttributeValueAsInt(const AttributeImpl *a) {
	return unwrap(a).getValueAsInt();
}

LLVMBool LLVM_Hs_IsStringAttribute(const AttributeImpl *a) {
	return unwrap(a).isStringAttribute();
}

const char *LLVM_Hs_AttributeKindAsString(const AttributeImpl *a, size_t &l) {
	const StringRef s = unwrap(a).getKindAsString();
	l = s.size();
	return s.data();
}

const char *LLVM_Hs_AttributeValueAsString(const AttributeImpl *a, size_t &l) {
	const StringRef s = unwrap(a).getValueAsString();
	l = s.size();
	return s.data();
}

const AttributeSetImpl *LLVM_Hs_GetAttributeSet(LLVMContextRef context, unsigned index, const AttrBuilder &ab) {
	return wrap(AttributeSet::get(*unwrap(context), index, ab));
}

const AttributeSetImpl *LLVM_Hs_MixAttributeSets(
	LLVMContextRef context, const AttributeSetImpl **as, unsigned n
) {
	return wrap(
		AttributeSet::get(
			*unwrap(context),
			ArrayRef<AttributeSet>(reinterpret_cast<const AttributeSet *>(as), n)
		)
	);
}

size_t LLVM_Hs_GetAttrBuilderSize() { return sizeof(AttrBuilder); }

AttrBuilder *LLVM_Hs_ConstructAttrBuilder(char *p) {
	return new(p) AttrBuilder();
}

void LLVM_Hs_DestroyAttrBuilder(AttrBuilder *a) {
	a->~AttrBuilder();
}

void LLVM_Hs_AttrBuilderAddAttributeKind(AttrBuilder &ab, unsigned kind) {
    ab.addAttribute(Attribute::AttrKind(kind));
}

void LLVM_Hs_AttrBuilderAddStringAttribute(
	AttrBuilder &ab, const char *kind, size_t kind_len, const char *value, size_t value_len
) {
	ab.addAttribute(StringRef(kind, kind_len), StringRef(value, value_len));
}

void LLVM_Hs_AttrBuilderAddAlignment(AttrBuilder &ab, uint64_t v) {
	ab.addAlignmentAttr(v);
}

void LLVM_Hs_AttrBuilderAddStackAlignment(AttrBuilder &ab, uint64_t v) {
	ab.addStackAlignmentAttr(v);
}

void LLVM_Hs_AttrBuilderAddAllocSize(AttrBuilder &ab, unsigned x, unsigned y, LLVMBool optionalIsThere) {
    if (optionalIsThere) {
        ab.addAllocSizeAttr(x, Optional<unsigned>(y));
    } else {
        ab.addAllocSizeAttr(x, Optional<unsigned>());
    }
}

void LLVM_Hs_AttrBuilderAddDereferenceableAttr(AttrBuilder &ab, uint64_t v) {
	ab.addDereferenceableAttr(v);
}

void LLVM_Hs_AttrBuilderAddDereferenceableOrNullAttr(AttrBuilder &ab, uint64_t v) {
    ab.addDereferenceableOrNullAttr(v);
}

LLVMBool LLVM_Hs_AttributeGetAllocSizeArgs(const AttributeImpl* a, unsigned* x, unsigned* y) {
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
