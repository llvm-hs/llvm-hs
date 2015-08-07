#define __STDC_LIMIT_MACROS
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Attributes.h"

using namespace llvm;

extern "C" {

AttrBuilder *LLVM_General_CreateAttrBuilder() {
	return new AttrBuilder();
}

void LLVM_General_DisposeAttrBuilder(AttrBuilder *ab) {
	delete ab;
}

}
