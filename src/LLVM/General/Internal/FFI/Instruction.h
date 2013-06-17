#ifndef __LLVM_GENERAL_INTERNAL_FFI__INSTRUCTION__H__
#define __LLVM_GENERAL_INTERNAL_FFI__INSTRUCTION__H__

#define LLVM_GENERAL_FOR_EACH_ATOMIC_ORDERING(macro) \
	macro(NotAtomic) \
	macro(Unordered) \
	macro(Monotonic) \
	macro(Acquire) \
	macro(Release) \
	macro(AcquireRelease) \
	macro(SequentiallyConsistent)

typedef enum {
#define ENUM_CASE(x) LLVM ## x ## AtomicOrdering,
LLVM_GENERAL_FOR_EACH_ATOMIC_ORDERING(ENUM_CASE)
#undef ENUM_CASE
} LLVMAtomicOrdering;

#define LLVM_GENERAL_FOR_EACH_SYNCRONIZATION_SCOPE(macro) \
	macro(SingleThread) \
	macro(CrossThread)

typedef enum {
#define ENUM_CASE(x) LLVM ## x ## SynchronizationScope,
LLVM_GENERAL_FOR_EACH_SYNCRONIZATION_SCOPE(ENUM_CASE)
#undef ENUM_CASE
} LLVMSynchronizationScope;

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

typedef enum {
#define ENUM_CASE(x) LLVM ## x ## RMWOperation,
LLVM_GENERAL_FOR_EACH_RMW_OPERATION(ENUM_CASE)
#undef ENUM_CASE
} LLVMAtomicRMWOperation;

#endif
