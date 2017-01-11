#define __STDC_LIMIT_MACROS
#include "llvm/AsmParser/Parser.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"

#include "llvm-c/Core.h"
#include "llvm-c/IRReader.h"

using namespace llvm;

extern "C" {

LLVMModuleRef LLVM_Hs_ParseLLVMAssembly(LLVMContextRef context,
                                             LLVMMemoryBufferRef memoryBuffer,
                                             char **error) {
  LLVMModuleRef M;
  LLVMParseIRInContext(context, memoryBuffer, &M, error);
  return M;
}

void LLVM_Hs_WriteLLVMAssembly(LLVMModuleRef module, raw_ostream &os) {
  os << *unwrap(module);
}
}
