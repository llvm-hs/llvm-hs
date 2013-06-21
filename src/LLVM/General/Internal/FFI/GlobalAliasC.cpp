#define __STDC_LIMIT_MACROS
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/GlobalAlias.h"

#include "llvm-c/Core.h"

#include <iostream>

using namespace llvm;

extern "C" {

LLVMValueRef LLVM_General_GetAliasee(LLVMValueRef g) {
	return wrap(unwrap<GlobalAlias>(g)->getAliasee());
}

void LLVM_General_SetAliasee(LLVMValueRef g, LLVMValueRef c) {
	unwrap<GlobalAlias>(g)->setAliasee(unwrap<Constant>(c));
}

}
