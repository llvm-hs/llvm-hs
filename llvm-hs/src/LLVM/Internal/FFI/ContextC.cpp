#define __STDC_LIMIT_MACROS
#include "llvm-c/Core.h"
#include "llvm/IR/LLVMContext.h"

using namespace llvm;

extern "C" {

void LLVM_Hs_SetOpaquePointers(LLVMContextRef context) {
  unwrap(context)->setOpaquePointers(true);
}

}
