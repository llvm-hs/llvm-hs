#ifndef __LLVM_INTERNAL_FFI__ORC_JIT__H__
#define __LLVM_INTERNAL_FFI__ORC_JIT__H__

#define LLVM_HS_FOR_EACH_JIT_SYMBOL_FLAG(macro) \
    macro(None)                                      \
    macro(HasError)                                  \
    macro(Weak)                                      \
    macro(Common)                                    \
    macro(Absolute)                                  \
    macro(Exported)                                  \

typedef enum {
              LLVMJITSymbolFlagNone = 0,
              LLVMJITSymbolFlagHasError = 1U << 0,
              LLVMJITSymbolFlagWeak = 1U << 1,
              LLVMJITSymbolFlagCommon = 1U << 2,
              LLVMJITSymbolFlagAbsolute = 1U << 3,
              LLVMJITSymbolFlagExported = 1U << 4
} LLVMJITSymbolFlags;

#endif
