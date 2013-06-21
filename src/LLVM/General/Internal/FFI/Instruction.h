#ifndef __LLVM_GENERAL_INTERNAL_FFI__INSTRUCTION__H__
#define __LLVM_GENERAL_INTERNAL_FFI__INSTRUCTION__H__

#include "llvm/Config/llvm-config.h"

#define LLVM_GENERAL_FOR_EACH_ATOMIC_ORDERING(macro) \
	macro(NotAtomic) \
	macro(Unordered) \
	macro(Monotonic) \
	macro(Acquire) \
	macro(Release) \
	macro(AcquireRelease) \
	macro(SequentiallyConsistent)

#define LLVM_GENERAL_FOR_EACH_RMW_OPERATION(macro) \
	macro(Xchg) \
	macro(Add) \
	macro(Sub) \
	macro(And) \
	macro(Nand) \
	macro(Or) \
	macro(Xor) \
	macro(Max) \
	macro(Min) \
	macro(UMax) \
	macro(UMin)

#define LLVM_GENERAL_FOR_EACH_SYNCRONIZATION_SCOPE(macro) \
	macro(SingleThread) \
	macro(CrossThread)

#if LLVM_VERSION_MAJOR < 3 || (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR < 3)

typedef enum {
#define ENUM_CASE(x) LLVMAtomicOrdering ## x,
LLVM_GENERAL_FOR_EACH_ATOMIC_ORDERING(ENUM_CASE)
#undef ENUM_CASE
} LLVMAtomicOrdering;

typedef enum {
#define ENUM_CASE(x) LLVMAtomicRMWBinOp ## x,
LLVM_GENERAL_FOR_EACH_RMW_OPERATION(ENUM_CASE)
#undef ENUM_CASE
} LLVMAtomicRMWBinOp;

#endif

typedef enum {
#define ENUM_CASE(x) LLVM ## x ## SynchronizationScope,
LLVM_GENERAL_FOR_EACH_SYNCRONIZATION_SCOPE(ENUM_CASE)
#undef ENUM_CASE
} LLVMSynchronizationScope;

#endif
