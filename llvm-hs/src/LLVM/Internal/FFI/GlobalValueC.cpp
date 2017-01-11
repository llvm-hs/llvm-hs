#define __STDC_LIMIT_MACROS
#include "llvm/IR/Comdat.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/GlobalObject.h"
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

unsigned LLVM_Hs_GetCOMDATSelectionKind(const Comdat &comdat) {
  return unsigned(comdat.getSelectionKind());
}

void LLVM_Hs_SetCOMDATSelectionKind(Comdat &comdat, unsigned csk) {
  comdat.setSelectionKind(Comdat::SelectionKind(csk));
}

static LLVMUnnamedAddr unwrap(GlobalValue::UnnamedAddr a) {
    switch (a) {
#define ENUM_CASE(x) case GlobalValue::UnnamedAddr::x: return LLVMUnnamedAddr ## x;
LLVM_HS_FOR_EACH_UNNAMED_ADDR(ENUM_CASE)
#undef ENUM_CASE
    default: return LLVMUnnamedAddrNone;
    }
}

static GlobalValue::UnnamedAddr wrap(LLVMUnnamedAddr a) {
    switch (a) {
#define ENUM_CASE(x) case LLVMUnnamedAddr ## x: return GlobalValue::UnnamedAddr::x;
LLVM_HS_FOR_EACH_UNNAMED_ADDR(ENUM_CASE)
#undef ENUM_CASE
    default: return GlobalValue::UnnamedAddr::None;
    }
}

LLVMUnnamedAddr LLVM_Hs_GetUnnamedAddr(LLVMValueRef globalVal) {
    return unwrap(unwrap<GlobalValue>(globalVal)->getUnnamedAddr());
}

void LLVM_Hs_SetUnnamedAddr(LLVMValueRef globalVal, LLVMUnnamedAddr attr) {
    unwrap<GlobalValue>(globalVal)->setUnnamedAddr(wrap(attr));
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

}
