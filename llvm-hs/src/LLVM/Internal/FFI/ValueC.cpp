#define __STDC_LIMIT_MACROS
#include "llvm-c/Core.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Argument.h"
#include "LLVM/Internal/FFI/Value.h"

using namespace llvm;

extern "C" {

LLVMValueSubclassId LLVM_Hs_GetValueSubclassId(LLVMValueRef v) {
	switch(unwrap(v)->getValueID()) {
#define VALUE_SUBCLASS_ID_CASE(class) case Value::class ## Val: return LLVM ## class ## SubclassId;
LLVM_HS_FOR_EACH_VALUE_SUBCLASS(VALUE_SUBCLASS_ID_CASE)
#undef VALUE_SUBCLASS_ID_CASE
	default: break;
	}
	return LLVMValueSubclassId(0);
}

LLVMValueRef LLVM_Hs_CreateArgument(
	LLVMTypeRef t,
	const char *name
) {
	return wrap(new Argument(unwrap(t), name));
}

}
