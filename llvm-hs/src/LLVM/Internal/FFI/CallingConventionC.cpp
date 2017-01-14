#include "LLVM/Internal/FFI/CallingConvention.h"
#include "llvm/IR/CallingConv.h"

#define CHECK(l, n)                                                            \
    static_assert(unsigned(llvm::CallingConv::l) ==                            \
                      unsigned(LLVM_Hs_CallingConvention_##l),            \
                  "LLVM_Hs_CallingConvention enum out of sync w/ "        \
                  "llvm::CallingConv::ID for " #l);
LLVM_HS_FOR_EACH_CALLING_CONVENTION(CHECK)
#undef CHECK
