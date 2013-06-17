#define __STDC_LIMIT_MACROS
#include "llvm/LLVMContext.h"
#include "llvm/Attributes.h"

#include "llvm-c/Core.h"

using namespace llvm;

extern "C" {

LLVMAttribute LLVM_General_GetFunctionRetAttr(LLVMValueRef f) {
	return (LLVMAttribute)unwrap<Function>(f)->getRetAttributes().Raw();
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
