#define __STDC_LIMIT_MACROS
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Value.h"

#include "llvm-c/Core.h"
#include "LLVM/General/Internal/FFI/AttributeC.hpp"
#include "LLVM/General/Internal/FFI/CallingConventionC.hpp"

using namespace llvm;

extern "C" {

LLVMAttribute LLVM_General_GetFunctionRetAttr(LLVMValueRef f) {
	return (LLVMAttribute)unwrap<Function>(f)->getAttributes().Raw(AttributeSet::ReturnIndex);
}

void LLVM_General_AddFunctionRetAttr(LLVMValueRef v, LLVMAttribute attr) {
	Function &f = *unwrap<Function>(v);
	LLVMContext &context = f.getContext();
	AttrBuilder attrBuilder(attr);
	f.setAttributes(
		f.getAttributes().addAttributes(context, AttributeSet::ReturnIndex, AttributeSet::get(context, AttributeSet::ReturnIndex, attrBuilder))
	);
}

const AttributeSetImpl *LLVM_General_GetFunctionMixedAttributeSet(LLVMValueRef f) {
	return wrap(unwrap<Function>(f)->getAttributes());
}

void LLVM_General_SetFunctionMixedAttributeSet(LLVMValueRef f, AttributeSetImpl *asi) {
	unwrap<Function>(f)->setAttributes(unwrap(asi));
}

LLVMBool LLVM_General_HasFunctionPrefixData(LLVMValueRef f) {
  return unwrap<Function>(f)->hasPrefixData();
}

LLVMValueRef LLVM_General_GetFunctionPrefixData(LLVMValueRef f) {
  return wrap(unwrap<Function>(f)->getPrefixData());
}

void LLVM_General_SetFunctionPrefixData(LLVMValueRef f, LLVMValueRef p) {
  unwrap<Function>(f)->setPrefixData(unwrap<Constant>(p));
}

unsigned LLVM_General_GetFunctionCallingConvention(LLVMValueRef f) {
  LLVM_General_CallingConventionEnumMatches();
  return unsigned(unwrap<Function>(f)->getCallingConv());
}

void LLVM_General_SetFunctionCallingConvention(LLVMValueRef f, unsigned cc) {
  LLVM_General_CallingConventionEnumMatches();
  unwrap<Function>(f)->setCallingConv(llvm::CallingConv::ID(cc));
}

// This wrapper is necessary because LLVMSetPersonalityFn fails if
// personalityFn is a nullptr even though the C++ API allows that.
void LLVM_General_SetPersonalityFn(LLVMValueRef fn, LLVMValueRef personalityFn) {
    unwrap<Function>(fn)->setPersonalityFn(personalityFn == nullptr ?
                                           nullptr : unwrap<Constant>(personalityFn));
}

}
