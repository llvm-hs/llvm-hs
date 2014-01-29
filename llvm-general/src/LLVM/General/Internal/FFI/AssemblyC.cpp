#define __STDC_LIMIT_MACROS
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Assembly/Parser.h"
#include "llvm/Assembly/PrintModulePass.h"
#include "llvm/Pass.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/MemoryBuffer.h"

#include "llvm-c/Core.h"

using namespace llvm;

extern "C" {

LLVMModuleRef LLVM_General_ParseLLVMAssembly(
	LLVMContextRef context,
	LLVMMemoryBufferRef memoryBuffer,
	SMDiagnostic *error
) {
	return wrap(ParseAssembly(unwrap(memoryBuffer), NULL, *error, *unwrap(context)));
}

void LLVM_General_WriteLLVMAssembly(LLVMModuleRef module, raw_ostream &os) {
	os << *unwrap(module);
}

}

