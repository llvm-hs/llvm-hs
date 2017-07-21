#ifndef __LLVM_INTERNAL_FFI__CALLING_CONVENTION__H__
#define __LLVM_INTERNAL_FFI__CALLING_CONVENTION__H__

#define LLVM_HS_FOR_EACH_CALLING_CONVENTION(macro) \
  macro(C, 0)                                           \
  macro(Fast, 8)                                        \
  macro(Cold, 9)                                        \
  macro(GHC, 10)                                        \
  macro(HiPE, 11)                                       \
  macro(WebKit_JS, 12)                                  \
  macro(AnyReg, 13)                                     \
  macro(PreserveMost, 14)                               \
  macro(PreserveAll, 15)                                \
  macro(X86_StdCall, 64)                                \
  macro(X86_FastCall, 65)                               \
  macro(ARM_APCS, 66)                                   \
  macro(ARM_AAPCS, 67)                                  \
  macro(ARM_AAPCS_VFP, 68)                              \
  macro(MSP430_INTR, 69)                                \
  macro(X86_ThisCall, 70)                               \
  macro(PTX_Kernel, 71)                                 \
  macro(PTX_Device, 72)                                 \
  macro(SPIR_FUNC, 75)                                  \
  macro(SPIR_KERNEL, 76)                                \
  macro(Intel_OCL_BI, 77)                               \
  macro(X86_64_SysV, 78)                                \
  macro(Win64, 79)

typedef enum {
#define ENUM_CASE(l,n) LLVM_Hs_CallingConvention_ ## l = n,
  LLVM_HS_FOR_EACH_CALLING_CONVENTION(ENUM_CASE)
#undef ENUM_CASE
} LLVM_Hs_CallingConvention;

#endif
