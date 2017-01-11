#ifndef __LLVM_INTERNAL_FFI__SMDIAGNOSTIC__
#define __LLVM_INTERNAL_FFI__SMDIAGNOSTIC__

#define LLVM_HS_FOR_EACH_DIAGNOSTIC_KIND(macro) \
	macro(Error) \
	macro(Warning) \
	macro(Note)

typedef enum {
#define ENUM_CASE(k) LLVMDiagnosticKind ## k,
LLVM_HS_FOR_EACH_DIAGNOSTIC_KIND(ENUM_CASE)
#undef ENUM_CASE
} LLVMDiagnosticKind;

#endif
