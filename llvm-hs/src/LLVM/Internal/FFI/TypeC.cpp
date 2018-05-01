#define __STDC_LIMIT_MACROS
#include "llvm-c/Core.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"

using namespace llvm;

extern "C" {

LLVMTypeRef LLVM_Hs_StructCreateNamed(LLVMContextRef C, const char *Name, char** renamedName) {
    if (Name) {
        auto t = StructType::create(*unwrap(C), Name);
        *renamedName = strdup(t->getName().str().c_str());
        return wrap(t);
    } else {
        *renamedName = nullptr;
        return wrap(StructType::create(*unwrap(C)));
    }
}

LLVMBool LLVM_Hs_StructIsLiteral(LLVMTypeRef t) {
    return unwrap<StructType>(t)->isLiteral();
}

LLVMBool LLVM_Hs_StructIsOpaque(LLVMTypeRef t) {
    return unwrap<StructType>(t)->isOpaque();
}

LLVMTypeRef LLVM_Hs_ArrayType(LLVMTypeRef ElementType, uint64_t ElementCount) {
    return wrap(ArrayType::get(unwrap(ElementType), ElementCount));
}

uint64_t LLVM_Hs_GetArrayLength(LLVMTypeRef ArrayTy) {
    return unwrap<ArrayType>(ArrayTy)->getNumElements();
}

LLVMTypeRef LLVM_Hs_MetadataTypeInContext(LLVMContextRef C) {
    return wrap(Type::getMetadataTy(*unwrap(C)));
}

LLVMTypeRef LLVM_Hs_TokenTypeInContext(LLVMContextRef C) {
    return wrap(Type::getTokenTy(*unwrap(C)));
}
}
