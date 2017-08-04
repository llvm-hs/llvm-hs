#define __STDC_LIMIT_MACROS
#include "llvm/IR/Attributes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Value.h"

#include "LLVM/Internal/FFI/AttributeC.hpp"
#include "llvm-c/Core.h"

using namespace llvm;

extern "C" {
void LLVM_Hs_SetFunctionAttributeList(LLVMValueRef f,
                                      LLVMAttributeListRef attrs) {
    unwrap<Function>(f)->setAttributes(*attrs);
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
    unwrap<Function>(fn)->setPersonalityFn(
        personalityFn == nullptr ? nullptr : unwrap<Constant>(personalityFn));
}

LLVMAttributeSetRef LLVM_Hs_FunctionAttributesAtIndex(LLVMValueRef fn,
                                                      LLVMAttributeIndex idx) {
    return new AttributeSet(
        unwrap<Function>(fn)->getAttributes().getAttributes(idx));
}
}
