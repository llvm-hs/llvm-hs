#define __STDC_LIMIT_MACROS
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/Function.h"

#include "llvm-c/Core.h"

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


}
