#define __STDC_LIMIT_MACROS
//#include "llvm/Config/llvm-config.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm-c/Core.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Bitcode/ReaderWriter.h"

using namespace llvm;

extern "C" {

LLVMModuleRef LLVM_General_ParseBitcode(
	LLVMContextRef c,
	LLVMMemoryBufferRef mb, 
	char **error
) {
	std::string msg;
	Module *m = ParseBitcodeFile(unwrap(mb), *unwrap(c), &msg);
	if (m == 0) *error = strdup(msg.c_str());
	return wrap(m);
}

void LLVM_General_WriteBitcode(LLVMModuleRef m, raw_ostream &os) {
	WriteBitcodeToFile(unwrap(m), os);
}

}

