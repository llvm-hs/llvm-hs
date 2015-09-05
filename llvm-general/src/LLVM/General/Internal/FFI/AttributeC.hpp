#ifndef __LLVM_GENERAL_ATTRIBUTE_C_HPP__
#define __LLVM_GENERAL_ATTRIBUTE_C_HPP__
#define __STDC_LIMIT_MACROS
#include "llvm/IR/Attributes.h"
#include "LLVM/General/Internal/FFI/Attribute.h"
using namespace llvm;

inline void LLVM_General_AttributeSetMatches() {
	static_assert(
		sizeof(AttributeSet) == sizeof(AttributeSetImpl *),
		"AttributeSet implementation has changed"
	);
}

inline AttributeSet unwrap(const AttributeSetImpl *asi) {
	LLVM_General_AttributeSetMatches();
	return *reinterpret_cast<const AttributeSet *>(&asi);
}

inline const AttributeSetImpl *wrap(AttributeSet as) {
	LLVM_General_AttributeSetMatches();
	return *reinterpret_cast<const AttributeSetImpl **>(&as);
}

inline void LLVM_General_AttributeMatches() {
	static_assert(
		sizeof(Attribute) == sizeof(AttributeImpl *),
		"Attribute implementation has changed"
	);
}

inline Attribute unwrap(const AttributeImpl *ai) {
	LLVM_General_AttributeMatches();
	return *reinterpret_cast<const Attribute *>(&ai);
}

inline const AttributeImpl *wrap(Attribute a) {
	LLVM_General_AttributeMatches();
	return *reinterpret_cast<const AttributeImpl **>(&a);
}

#endif
