#define __STDC_LIMIT_MACROS
#include "llvm/IR/LLVMContext.h"
#include "llvm-c/ExecutionEngine.h"

using namespace llvm;

extern "C" {

size_t LLVM_General_GetMCJITCompilerOptionsSize() {
  return sizeof(struct LLVMMCJITCompilerOptions);
}

void LLVM_General_SetMCJITCompilerOptionsOptLevel(struct LLVMMCJITCompilerOptions *o, unsigned x) {
  o->OptLevel = x;
}

void LLVM_General_SetMCJITCompilerOptionsCodeModel(struct LLVMMCJITCompilerOptions *o, LLVMCodeModel x) {
  o->CodeModel = x;
}

void LLVM_General_SetMCJITCompilerOptionsNoFramePointerElim(struct LLVMMCJITCompilerOptions *o, LLVMBool x) {
  o->NoFramePointerElim = x;
}

void LLVM_General_SetMCJITCompilerOptionsEnableFastISel(struct LLVMMCJITCompilerOptions *o, LLVMBool x) {
  o->EnableFastISel = x;
}

}

