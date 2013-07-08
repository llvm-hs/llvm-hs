#define __STDC_LIMIT_MACROS
#include "llvm-c/Core.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"

using namespace llvm;

extern "C" {

LLVMTypeRef LLVM_General_StructCreateNamed(LLVMContextRef C, const char *Name) {
	if (Name) {
		return wrap(StructType::create(*unwrap(C), Name));
	} else {
		return wrap(StructType::create(*unwrap(C)));
	}
}

LLVMBool LLVM_General_StructIsLiteral(LLVMTypeRef t) {
	return unwrap<StructType>(t)->isLiteral();
}

LLVMBool LLVM_General_StructIsOpaque(LLVMTypeRef t) {
	return unwrap<StructType>(t)->isOpaque();
}

LLVMTypeRef LLVM_General_ArrayType(LLVMTypeRef ElementType, uint64_t ElementCount) {
	return wrap(ArrayType::get(unwrap(ElementType), ElementCount));
}

uint64_t LLVM_General_GetArrayLength(LLVMTypeRef ArrayTy) {
	return unwrap<ArrayType>(ArrayTy)->getNumElements();
}

LLVMTypeRef LLVM_General_MetadataTypeInContext(LLVMContextRef C) {
  return wrap(Type::getMetadataTy(*unwrap(C)));
}

}
