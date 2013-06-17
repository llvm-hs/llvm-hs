#ifndef __LLVM_GENERAL_INTERNAL_FFI__INLINE_ASSEMBLY__H__
#define __LLVM_GENERAL_INTERNAL_FFI__INLINE_ASSEMBLY__H__

#define LLVM_GENERAL_FOR_EACH_ASM_DIALECT(macro) \
	macro(ATT) \
	macro(Intel) \

typedef enum {
#define ENUM_CASE(d) LLVMAsmDialect_ ## d,
	LLVM_GENERAL_FOR_EACH_ASM_DIALECT(ENUM_CASE)
#undef ENUM_CASE
} LLVMAsmDialect;

#endif
