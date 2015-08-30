#define __STDC_LIMIT_MACROS
#include <iostream>
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Attributes.h"
#include "LLVM/General/Internal/FFI/Attributes.h"

using namespace llvm;
using namespace std;

extern "C" {

AttrBuilder *LLVM_General_CreateAttrBuilder() {
	return new AttrBuilder();
}

void LLVM_General_DisposeAttrBuilder(AttrBuilder *ab) {
	delete ab;
}

unsigned LLVM_General_AttributeSetNumSlots(AttributeSetImpl *a) {
	return reinterpret_cast<AttributeSet *>(&a)->getNumSlots();
}

int LLVM_General_AttributeSetSlotIndex(AttributeSetImpl *a, unsigned slot) {
	return reinterpret_cast<AttributeSet *>(&a)->getSlotIndex(slot);
}

AttributeSetImpl *LLVM_General_AttributeSetSlotAttributes(AttributeSetImpl *a, unsigned slot) {
	AttributeSet result = reinterpret_cast<AttributeSet *>(&a)->getSlotAttributes(slot);
	return *reinterpret_cast<AttributeSetImpl **>(&result);
}

const AttributeImpl *const *LLVM_General_AttributeSetGetAttributes(AttributeSetImpl *a, unsigned slot, unsigned *length) {
	AttributeSet &as = *reinterpret_cast<AttributeSet *>(&a);
	ArrayRef<Attribute>::iterator b = as.begin(slot), e = as.end(slot);
	*length = e - b;
	return reinterpret_cast<const AttributeImpl *const *>(b);
}

constexpr bool LLVM_General_AttributeEnumMatches() {
	return true;
}

unsigned LLVM_General_AttributeEnum(const AttributeImpl *a) {
#define CHECK(name,p,r,f)																								\
	static_assert(																												\
		unsigned(llvm::Attribute::name) == unsigned(LLVM_General_AttributeKind_ ## name), \
		"LLVM_General_AttributeKind enum out of sync w/ llvm::Attribute::AttrKind for " #name	\
	);
	LLVM_GENERAL_FOR_EACH_ATTRIBUTE_KIND(CHECK)
#undef CHECK
	return reinterpret_cast<Attribute *>(&a)->getKindAsEnum();
};

uint64_t LLVM_General_AttributeValueAsInt(const AttributeImpl *a) {
	return reinterpret_cast<Attribute *>(&a)->getValueAsInt();
};

}
