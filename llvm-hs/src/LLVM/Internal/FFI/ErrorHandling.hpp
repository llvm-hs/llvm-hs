#ifndef __LLVM_INTERNAL_FFI__ERRORHANDLING__H__
#define __LLVM_INTERNAL_FFI__ERRORHANDLING__H__
#include <string>

[[noreturn]] void reportFatalError(const std::string &errorMsg);

#endif
