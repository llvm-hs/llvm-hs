#ifndef __LLVM_GENERAL_INTERNAL_FFI__ANALYSIS__H__
#define __LLVM_GENERAL_INTERNAL_FFI__ANALYSIS__H__

#include "llvm-c/Analysis.h"

#define LLVM_GENERAL_FOR_EACH_VERIFIER_FAILURE_ACTION(macro) \
	macro(AbortProcess) \
	macro(PrintMessage) \
	macro(ReturnStatus)

#endif
