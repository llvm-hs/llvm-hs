#define __STDC_LIMIT_MACROS
#include "llvm/IR/GlobalValue.h"
#include "llvm-c/Core.h"
#include "LLVM/General/Internal/FFI/GlobalValue.h"

using namespace llvm;

extern "C" {

LLVMBool LLVM_General_HasUnnamedAddr(LLVMValueRef globalVal) {
	return unwrap<GlobalValue>(globalVal)->hasUnnamedAddr();
}

void LLVM_General_SetUnnamedAddr(LLVMValueRef globalVal, LLVMBool isUnnamedAddr) {
	unwrap<GlobalValue>(globalVal)->setUnnamedAddr(isUnnamedAddr);
}

inline void LLVM_General_TLS_Model_Enum_Matches() {
#define ENUM_CASE(n) static_assert(unsigned(GlobalValue::n) == unsigned(LLVM ## n), "TLS Model Enum mismatch");
	LLVM_GENERAL_FOR_EACH_THREAD_LOCAL_MODE(ENUM_CASE)
#undef ENUM_CASE
}

LLVMThreadLocalMode LLVM_General_GetThreadLocalMode(LLVMValueRef globalVal) {
	LLVM_General_TLS_Model_Enum_Matches();
	return LLVMThreadLocalMode(unwrap<GlobalValue>(globalVal)->getThreadLocalMode());
}

void LLVM_General_SetThreadLocalMode(LLVMValueRef globalVal, LLVMThreadLocalMode mode) {
	LLVM_General_TLS_Model_Enum_Matches();
	unwrap<GlobalValue>(globalVal)->setThreadLocalMode(GlobalValue::ThreadLocalMode(mode));
}

}
