#include "LLVM/Internal/FFI/ErrorHandling.hpp"
#include <iostream>

void reportFatalError(const std::string &errorMsg) {
    std::cerr << "LLVM-HS ERROR at " << __FILE__ << ":" << __LINE__ << ": "
              << errorMsg << "\n ";
    exit(1);
}
