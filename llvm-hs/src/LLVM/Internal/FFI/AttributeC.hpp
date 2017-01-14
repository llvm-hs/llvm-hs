#ifndef __LLVM_ATTRIBUTE_C_HPP__
#define __LLVM_ATTRIBUTE_C_HPP__
#define __STDC_LIMIT_MACROS
#include "llvm/IR/Attributes.h"
#include "LLVM/Internal/FFI/Attribute.h"
using namespace llvm;

inline AttributeSet unwrap(const AttributeSetImpl *asi) {
    return *reinterpret_cast<const AttributeSet *>(&asi);
}

inline const AttributeSetImpl *wrap(AttributeSet as) {
    return *reinterpret_cast<const AttributeSetImpl **>(&as);
}

inline Attribute unwrap(const AttributeImpl *ai) {
    return *reinterpret_cast<const Attribute *>(&ai);
}

inline const AttributeImpl *wrap(Attribute a) {
    return *reinterpret_cast<const AttributeImpl **>(&a);
}

#endif
