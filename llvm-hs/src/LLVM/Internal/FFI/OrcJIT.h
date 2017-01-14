#ifndef __LLVM_INTERNAL_FFI__ORC_JIT__H__
#define __LLVM_INTERNAL_FFI__ORC_JIT__H__

#define LLVM_HS_FOR_EACH_JIT_SYMBOL_FLAG(macro) \
	macro(None)                                      \
	macro(Weak)                                      \
	macro(Exported)                                  \

typedef enum {
#define ENUM_CASE(x) LLVMJITSymbolFlag ## x,
LLVM_HS_FOR_EACH_JIT_SYMBOL_FLAG(ENUM_CASE)
#undef ENUM_CASE
} LLVMJITSymbolFlags;

#endif
