#define __STDC_LIMIT_MACROS

#include "llvm/ExecutionEngine/RTDyldMemoryManager.h"

using namespace llvm;

extern "C" {

uint64_t LLVM_Hs_GetSymbolAddressInProcess(const char *name) {
    std::string nameStr(name);
    return RTDyldMemoryManager::getSymbolAddressInProcess(nameStr);
}

}
