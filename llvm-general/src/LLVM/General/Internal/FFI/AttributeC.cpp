#define __STDC_LIMIT_MACROS
#include "llvm/IR/LLVMContext.h"
#include "LLVM/General/Internal/FFI/AttributeC.hpp"

extern "C" {

unsigned LLVM_General_AttributeSetNumSlots(const AttributeSetImpl *a) {
	return unwrap(a).getNumSlots();
}

int LLVM_General_AttributeSetSlotIndex(const AttributeSetImpl *a, unsigned slot) {
	return unwrap(a).getSlotIndex(slot);
}

const AttributeSetImpl *LLVM_General_AttributeSetSlotAttributes(const AttributeSetImpl *a, unsigned slot) {
	return wrap(unwrap(a).getSlotAttributes(slot));
}

const AttributeSetImpl *LLVM_General_GetSlotAttributeSet(
	LLVMContextRef context,
	unsigned index,
	const AttributeImpl **attributes,
	unsigned length
) {
	AttrBuilder builder;
	for (unsigned i = 0; i < length; i++) builder.addAttribute(unwrap(attributes[i]));
	return wrap(AttributeSet::get(*unwrap(context), index, builder));
}

const AttributeImpl *const *LLVM_General_AttributeSetGetAttributes(AttributeSetImpl *asi, unsigned slot, unsigned *length) {
	AttributeSet as = unwrap(asi);
	ArrayRef<Attribute>::iterator b = as.begin(slot), e = as.end(slot);
	*length = e - b;
	return reinterpret_cast<const AttributeImpl *const *>(b);
}

inline void LLVM_General_AttributeEnumMatches() {
#define CHECK(name,p,r,f)																								\
	static_assert(																												\
		unsigned(llvm::Attribute::name) == unsigned(LLVM_General_AttributeKind_ ## name), \
		"LLVM_General_AttributeKind enum out of sync w/ llvm::Attribute::AttrKind for " #name	\
	);
	LLVM_GENERAL_FOR_EACH_ATTRIBUTE_KIND(CHECK)
#undef CHECK
}

unsigned LLVM_General_AttributeKindAsEnum(const AttributeImpl *a) {
	LLVM_General_AttributeEnumMatches();
	return unwrap(a).getKindAsEnum();
}

uint64_t LLVM_General_AttributeValueAsInt(const AttributeImpl *a) {
	return unwrap(a).getValueAsInt();
}

LLVMBool LLVM_General_IsStringAttribute(const AttributeImpl *a) {
	return unwrap(a).isStringAttribute();
}

const char *LLVM_General_AttributeKindAsString(const AttributeImpl *a, size_t &l) {
	const StringRef s = unwrap(a).getKindAsString();
	l = s.size();
	return s.data();
}

const char *LLVM_General_AttributeValueAsString(const AttributeImpl *a, size_t &l) {
	const StringRef s = unwrap(a).getValueAsString();
	l = s.size();
	return s.data();
}

const AttributeSetImpl *LLVM_General_GetAttributeSet(LLVMContextRef context, unsigned index, const AttrBuilder &ab) {
	return wrap(AttributeSet::get(*unwrap(context), index, ab));
}

const AttributeSetImpl *LLVM_General_MixAttributeSets(
	LLVMContextRef context, const AttributeSetImpl **as, unsigned n
) {
	return wrap(
		AttributeSet::get(
			*unwrap(context),
			ArrayRef<AttributeSet>(reinterpret_cast<const AttributeSet *>(as), n)
		)
	);
}

size_t LLVM_General_GetAttrBuilderSize() { return sizeof(AttrBuilder); }

AttrBuilder *LLVM_General_ConstructAttrBuilder(char *p) {
	return new(p) AttrBuilder();
}

void LLVM_General_DestroyAttrBuilder(AttrBuilder *a) {
	a->~AttrBuilder();
}

void LLVM_General_AttrBuilderAddAttributeKind(AttrBuilder &ab, unsigned kind) {
	LLVM_General_AttributeEnumMatches();
	ab.addAttribute(Attribute::AttrKind(kind));
}

void LLVM_General_AttrBuilderAddStringAttribute(
	AttrBuilder &ab, const char *kind, size_t kind_len, const char *value, size_t value_len
) {
	ab.addAttribute(StringRef(kind, kind_len), StringRef(value, value_len));
}

void LLVM_General_AttrBuilderAddAlignment(AttrBuilder &ab, uint64_t v) {
	ab.addAlignmentAttr(v);
}

void LLVM_General_AttrBuilderAddStackAlignment(AttrBuilder &ab, uint64_t v) {
	ab.addStackAlignmentAttr(v);
}

void LLVM_General_AttrBuilderAddAllocSize(AttrBuilder &ab, unsigned x, LLVMBool optionalIsThere, unsigned y) {
    if (optionalIsThere) {
        ab.addAllocSizeAttr(x, y);
    } else {
        ab.addAllocSizeAttr(x, Optional<unsigned>());
    }
}

void LLVM_General_AttrBuilderAddDereferenceableAttr(AttrBuilder &ab, uint64_t v) {
	ab.addDereferenceableAttr(v);
}

void LLVM_General_AttrBuilderAddDereferenceableOrNullAttr(AttrBuilder &ab, uint64_t v) {
    ab.addDereferenceableOrNullAttr(v);
}

LLVMBool LLVM_General_AttributeGetAllocSizeArgs(const AttributeImpl* a, unsigned* x, unsigned* y) {
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
