#include "LLVM/General/Internal/FFI/CallingConvention.h"
#include "llvm/IR/CallingConv.h"

#define CHECK(l, n)                                                            \
    static_assert(unsigned(llvm::CallingConv::l) ==                            \
                      unsigned(LLVM_General_CallingConvention_##l),            \
                  "LLVM_General_CallingConvention enum out of sync w/ "        \
                  "llvm::CallingConv::ID for " #l);
LLVM_GENERAL_FOR_EACH_CALLING_CONVENTION(CHECK)
#undef CHECK
