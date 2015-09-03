#define __STDC_LIMIT_MACROS
#include <iostream>
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Attributes.h"
#include "LLVM/General/Internal/FFI/Attribute.h"

using namespace llvm;
using namespace std;

AttributeSet unwrap(const AttributeSetImpl *asi) {
	return *reinterpret_cast<const AttributeSet *>(&asi);
}

const AttributeSetImpl *wrap(AttributeSet as) {
	return *reinterpret_cast<const AttributeSetImpl **>(&as);
}

Attribute unwrap(const AttributeImpl *ai) {
	return *reinterpret_cast<const Attribute *>(&ai);
}

const AttributeImpl *wrap(Attribute a) {
	return *reinterpret_cast<const AttributeImpl **>(&a);
}

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

const AttributeImpl *LLVM_General_GetAttribute(LLVMContextRef context, unsigned kind, uint64_t value) {
	LLVM_General_AttributeEnumMatches();
	return wrap(Attribute::get(*unwrap(context), Attribute::AttrKind(kind), value));
}

const AttributeImpl *LLVM_General_GetStringAttribute(
	LLVMContextRef context, const char *kind, size_t kind_len, const char *value, size_t value_len
) {
	LLVM_General_AttributeEnumMatches();
	return wrap(Attribute::get(*unwrap(context), StringRef(kind, kind_len), StringRef(value, value_len)));
}

}
