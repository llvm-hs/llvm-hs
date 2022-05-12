#define __STDC_LIMIT_MACROS
#include "llvm-c/Core.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"

using namespace llvm;

extern "C" {

// LLVM may rename the type, which we must capture.
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

LLVMTypeRef LLVM_Hs_ArrayType(LLVMTypeRef ElementType, uint64_t ElementCount) {
    return wrap(ArrayType::get(unwrap(ElementType), ElementCount));
}

LLVMTypeRef LLVM_Hs_OpaquePointerType(LLVMContextRef C, unsigned AddrSpace) {
    return wrap(PointerType::get(*unwrap(C), AddrSpace));
}

LLVMBool LLVM_Hs_IsOpaquePointerType(LLVMTypeRef type) {
    return unwrap(type)->isOpaquePointerTy();
}

uint64_t LLVM_Hs_GetArrayLength(LLVMTypeRef ArrayTy) {
    return unwrap<ArrayType>(ArrayTy)->getNumElements();
}

}
