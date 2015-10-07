#ifndef __LLVM_GENERAL_INTERNAL_FFI__CALLING_CONVENTION_C_HPP__
#define __LLVM_GENERAL_INTERNAL_FFI__CALLING_CONVENTION_C_HPP__

#include "LLVM/General/Internal/FFI/CallingConvention.h"

inline void LLVM_General_CallingConventionEnumMatches() {
#define CHECK(l,n)                                             \
  static_assert(                                                        \
    unsigned(llvm::CallingConv::l) == unsigned(LLVM_General_CallingConvention_ ## l), \
    "LLVM_General_CallingConvention enum out of sync w/ llvm::CallingConv::ID for " #l  \
  );
  LLVM_GENERAL_FOR_EACH_CALLING_CONVENTION(CHECK)
#undef CHECK
}

#endif
