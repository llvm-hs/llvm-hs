#define __STDC_LIMIT_MACROS
#include "llvm/Config/llvm-config.h"
#if LLVM_VERSION_MAJOR < 3 || (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR < 3)
#include "llvm/LLVMContext.h"
#include "llvm/Attributes.h"
#else
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/Function.h"
#endif


#include "llvm-c/Core.h"

using namespace llvm;

extern "C" {

LLVMAttribute LLVM_General_GetFunctionRetAttr(LLVMValueRef f) {
	return (LLVMAttribute)unwrap<Function>(f)->getAttributes().getRetAttributes().Raw();
}

void LLVM_General_AddFunctionRetAttr(LLVMValueRef v, LLVMAttribute attr) {
	Function &f = *unwrap<Function>(v);
	LLVMContext &context = f.getContext();
	AttrBuilder attrBuilder(attr);
	f.setAttributes(
		f.getAttributes().addAttr(context, AttrListPtr::ReturnIndex, Attributes::get(context, attrBuilder))
	);
}


}
