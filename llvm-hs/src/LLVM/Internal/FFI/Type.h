#ifndef __LLVM_INTERNAL_FFI__TYPE__H__
#define __LLVM_INTERNAL_FFI__TYPE__H__

#define LLVM_HS_FOR_EACH_TYPE_KIND(macro) \
	macro(Void) \
	macro(Half) \
	macro(Float) \
	macro(Double) \
	macro(X86_FP80) \
	macro(FP128) \
	macro(PPC_FP128) \
	macro(Label) \
	macro(Integer) \
	macro(Function) \
	macro(Struct) \
	macro(Array) \
	macro(Pointer) \
	macro(Vector) \
	macro(Metadata) \
	macro(X86_MMX) \
	macro(Token)

#endif
