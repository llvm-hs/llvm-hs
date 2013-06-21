#define __STDC_LIMIT_MACROS
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/Function.h"
#include "llvm-c/Core.h"

#include "LLVM/General/Internal/FFI/InlineAssembly.h"

using namespace llvm;

namespace llvm {
static LLVMAsmDialect wrap(InlineAsm::AsmDialect d) {
	switch(d) {
#define ENUM_CASE(x) case InlineAsm::AD_ ## x: return LLVMAsmDialect_ ## x;
LLVM_GENERAL_FOR_EACH_ASM_DIALECT(ENUM_CASE)
#undef ENUM_CASE
	default: return LLVMAsmDialect(0);
	}
}

static InlineAsm::AsmDialect unwrap(LLVMAsmDialect d) {
	switch(d) {
#define ENUM_CASE(x) case LLVMAsmDialect_ ## x: return InlineAsm::AD_ ## x;
LLVM_GENERAL_FOR_EACH_ASM_DIALECT(ENUM_CASE)
#undef ENUM_CASE
	default: return InlineAsm::AsmDialect(0);
	}
}
}

extern "C" {

LLVMValueRef LLVM_General_CreateInlineAsm(
	LLVMTypeRef t,
	const char *asmStr,
	const char *constraintsStr,
	LLVMBool hasSideEffects,
	LLVMBool isAlignStack,
	LLVMAsmDialect dialect
) {
	return wrap(
		InlineAsm::get(
			unwrap<FunctionType>(t), 
			asmStr,
			constraintsStr,
			hasSideEffects,
			isAlignStack,
			unwrap(dialect)
		)
	);
}

const char *LLVM_General_GetInlineAsmAsmString(LLVMValueRef v) {
	return unwrap<InlineAsm>(v)->getAsmString().c_str();
}

const char *LLVM_General_GetInlineAsmConstraintString(LLVMValueRef v) {
	return unwrap<InlineAsm>(v)->getConstraintString().c_str();
}

LLVMBool LLVM_General_InlineAsmHasSideEffects(LLVMValueRef v) {
	return unwrap<InlineAsm>(v)->hasSideEffects();
}

LLVMBool LLVM_General_InlineAsmIsAlignStack(LLVMValueRef v) {
	return unwrap<InlineAsm>(v)->isAlignStack();
}

LLVMAsmDialect LLVM_General_GetInlineAsmDialect(LLVMValueRef v) {
	return wrap(unwrap<InlineAsm>(v)->getDialect());
}

}

