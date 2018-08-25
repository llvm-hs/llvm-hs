#define __STDC_LIMIT_MACROS
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm-c/Core.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Bitcode/BitcodeReader.h"
#include "llvm/Bitcode/BitcodeWriter.h"

using namespace llvm;

extern "C" {

LLVMModuleRef LLVM_Hs_ParseBitcode(
	LLVMContextRef c,
	LLVMMemoryBufferRef mb, 
	char **error
) {
    Expected<std::unique_ptr<Module>> moduleOrErr = parseBitcodeFile(unwrap(mb)->getMemBufferRef(), *unwrap(c));
    if (Error err = moduleOrErr.takeError()) {
        handleAllErrors(std::move(err), [&](ErrorInfoBase &eib) {
                *error = strdup(eib.message().c_str());
            });
        return nullptr;
    }
    return wrap(moduleOrErr.get().release());
}

void LLVM_Hs_WriteBitcode(LLVMModuleRef m, raw_ostream &os) {
	WriteBitcodeToFile(*unwrap(m), os);
}

}

