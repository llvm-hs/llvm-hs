#define __STDC_LIMIT_MACROS
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Value.h"

#include "llvm-c/Core.h"
#include "LLVM/Internal/FFI/AttributeC.hpp"

using namespace llvm;

extern "C" {

const AttributeSetImpl *LLVM_Hs_GetFunctionMixedAttributeSet(LLVMValueRef f) {
	return wrap(unwrap<Function>(f)->getAttributes());
}

void LLVM_Hs_SetFunctionMixedAttributeSet(LLVMValueRef f, AttributeSetImpl *asi) {
	unwrap<Function>(f)->setAttributes(unwrap(asi));
}

LLVMBool LLVM_Hs_HasFunctionPrefixData(LLVMValueRef f) {
  return unwrap<Function>(f)->hasPrefixData();
}

LLVMValueRef LLVM_Hs_GetFunctionPrefixData(LLVMValueRef f) {
  return wrap(unwrap<Function>(f)->getPrefixData());
}

void LLVM_Hs_SetFunctionPrefixData(LLVMValueRef f, LLVMValueRef p) {
  unwrap<Function>(f)->setPrefixData(unwrap<Constant>(p));
}

// This wrapper is necessary because LLVMSetPersonalityFn fails if
// personalityFn is a nullptr even though the C++ API allows that.
void LLVM_Hs_SetPersonalityFn(LLVMValueRef fn, LLVMValueRef personalityFn) {
    unwrap<Function>(fn)->setPersonalityFn(personalityFn == nullptr ?
                                           nullptr : unwrap<Constant>(personalityFn));
}

}
