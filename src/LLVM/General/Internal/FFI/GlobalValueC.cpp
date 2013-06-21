#define __STDC_LIMIT_MACROS
#include "llvm/IR/GlobalValue.h"
#include "llvm-c/Core.h"

using namespace llvm;

extern "C" {

LLVMBool LLVM_General_HasUnnamedAddr(LLVMValueRef globalVal) {
	return unwrap<GlobalValue>(globalVal)->hasUnnamedAddr();
}

void LLVM_General_SetUnnamedAddr(LLVMValueRef globalVal, LLVMBool isUnnamedAddr) {
	unwrap<GlobalValue>(globalVal)->setUnnamedAddr(isUnnamedAddr);
}

}
