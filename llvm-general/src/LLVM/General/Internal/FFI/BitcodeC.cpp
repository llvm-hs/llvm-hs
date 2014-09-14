#define __STDC_LIMIT_MACROS
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
	ErrorOr<Module *> m = parseBitcodeFile(unwrap(mb), *unwrap(c));
	if (error_code ec = m.getError()) {
		*error = strdup(ec.message().c_str());
		return 0;
	}
	return wrap(m.get());
}

void LLVM_General_WriteBitcode(LLVMModuleRef m, raw_ostream &os) {
	WriteBitcodeToFile(unwrap(m), os);
}

}

