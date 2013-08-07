#ifndef __LLVM_GENERAL_INTERNAL_FFI__SMDIAGNOSTIC__
#define __LLVM_GENERAL_INTERNAL_FFI__SMDIAGNOSTIC__

#define LLVM_GENERAL_FOR_EACH_DIAGNOSTIC_KIND(macro) \
	macro(Error) \
	macro(Warning) \
	macro(Note)

typedef enum {
#define ENUM_CASE(k) LLVMDiagnosticKind ## k,
LLVM_GENERAL_FOR_EACH_DIAGNOSTIC_KIND(ENUM_CASE)
#undef ENUM_CASE
} LLVMDiagnosticKind;

#endif
