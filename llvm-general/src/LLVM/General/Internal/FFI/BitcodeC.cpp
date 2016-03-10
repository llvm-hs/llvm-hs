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
	ErrorOr<std::unique_ptr<Module>> m = parseBitcodeFile(unwrap(mb)->getMemBufferRef(), *unwrap(c));
	if (std::error_code ec = m.getError()) {
		*error = strdup(ec.message().c_str());
		return 0;
	}
	return wrap(m.get().release());
}

void LLVM_General_WriteBitcode(LLVMModuleRef m, raw_ostream &os) {
	WriteBitcodeToFile(unwrap(m), os);
}

}

