#define __STDC_LIMIT_MACROS
#include "llvm/IR/Comdat.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/GlobalObject.h"
#include "llvm/IR/Metadata.h"
#include "llvm-c/Core.h"
#include "LLVM/Internal/FFI/GlobalValue.h"

using namespace llvm;

extern "C" {

const Comdat *LLVM_Hs_GetCOMDAT(LLVMValueRef globalVal) {
  return unwrap<GlobalValue>(globalVal)->getComdat();
}

void LLVM_Hs_SetCOMDAT(LLVMValueRef globalObj, Comdat *comdat) {
  return unwrap<GlobalObject>(globalObj)->setComdat(comdat);
}

const char *LLVM_Hs_GetCOMDATName(const Comdat &comdat, size_t &size) {
  StringRef ref = comdat.getName();
  size = ref.size();
  return ref.data();
}

#define ENUM_CASE(n)                                                           \
    static_assert(unsigned(Comdat::n) ==                                       \
                      unsigned(LLVM_Hs_COMDAT_Selection_Kind_##n),        \
                  "COMDAT SelectionKind Enum mismatch");
LLVM_HS_FOR_EACH_COMDAT_SELECTION_KIND(ENUM_CASE)
#undef ENUM_CASE

const char* LLVM_Hs_GetSection(LLVMValueRef globalVal, size_t* strLength) {
    const auto& section = unwrap<GlobalValue>(globalVal)->getSection();
    *strLength = section.size();
    return section.data();
}

unsigned LLVM_Hs_GetCOMDATSelectionKind(const Comdat &comdat) {
  return unsigned(comdat.getSelectionKind());
}

void LLVM_Hs_SetCOMDATSelectionKind(Comdat &comdat, unsigned csk) {
  comdat.setSelectionKind(Comdat::SelectionKind(csk));
}

#define ENUM_CASE(n)                                                           \
    static_assert(unsigned(GlobalValue::n) == unsigned(LLVM##n),               \
                  "TLS Model Enum mismatch");
LLVM_HS_FOR_EACH_THREAD_LOCAL_MODE(ENUM_CASE)
#undef ENUM_CASE

LLVMThreadLocalMode LLVM_Hs_GetThreadLocalMode(LLVMValueRef globalVal) {
    return LLVMThreadLocalMode(unwrap<GlobalValue>(globalVal)->getThreadLocalMode());
}

void LLVM_Hs_SetThreadLocalMode(LLVMValueRef globalVal, LLVMThreadLocalMode mode) {
    unwrap<GlobalValue>(globalVal)->setThreadLocalMode(GlobalValue::ThreadLocalMode(mode));
}

unsigned LLVM_Hs_GlobalObject_GetNumMetadata(GlobalObject* obj) {
    SmallVector<std::pair<unsigned, MDNode *>, 4> mds;
    obj->getAllMetadata(mds);
    return mds.size();
}

void LLVM_Hs_GlobalObject_GetAllMetadata(GlobalObject* obj, unsigned *kinds, LLVMMetadataRef *nodes) {
    SmallVector<std::pair<unsigned, MDNode*>, 4> mds;
    obj->getAllMetadata(mds);
    for (unsigned i = 0; i < mds.size(); ++i) {
        kinds[i] = mds[i].first;
        nodes[i] = wrap(mds[i].second);
    }
}

void LLVM_Hs_GlobalObject_SetMetadata(GlobalObject* obj, unsigned kind, MDNode* node) {
    obj->setMetadata(kind, node);
}

}
