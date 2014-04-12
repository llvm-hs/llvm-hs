#ifndef __LLVM_GENERAL_INTERNAL_FFI__BINARY_OPERATOR__H__
#define __LLVM_GENERAL_INTERNAL_FFI__BINARY_OPERATOR__H__

#define LLVM_GENERAL_FOR_EACH_POSSIBLY_EXACT_BINARY_OPERATOR(macro) \
	macro(UDiv) \
	macro(SDiv) \
	macro(LShr) \
	macro(AShr)

#define LLVM_GENERAL_FOR_EACH_OVERFLOWING_BINARY_OPERATOR(macro) \
	macro(Add) \
	macro(Mul) \
	macro(Shl) \
	macro(Sub) \

#endif
