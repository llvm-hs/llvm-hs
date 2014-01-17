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

LLVMBool LLVM_General_WriteBitcodeToFile(LLVMModuleRef m, const char *path, char **error) {
	std::string ErrorInfo;
	raw_fd_ostream OS(path, ErrorInfo, sys::fs::F_Binary);

	if (!ErrorInfo.empty()) {
		*error = strdup(ErrorInfo.c_str());
		return 1;
	}

	WriteBitcodeToFile(unwrap(m), OS);
	return 0;
}

void LLVM_General_GetModuleBitcode(LLVMModuleRef m, void (*callback)(const char *start, size_t length)) {
	std::string buf;
	{
		raw_string_ostream OS(buf);
		WriteBitcodeToFile(unwrap(m), OS);
	}
	(*callback)(buf.data(), buf.length());
}

}

