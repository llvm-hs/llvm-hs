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

typedef enum {
#define ENUM_CASE(x) LLVM ## x ## SynchronizationScope,
LLVM_GENERAL_FOR_EACH_SYNCRONIZATION_SCOPE(ENUM_CASE)
#undef ENUM_CASE
} LLVMSynchronizationScope;

#define LLVM_GENERAL_FOR_EACH_FAST_MATH_FLAG(macro) \
	macro(UnsafeAlgebra, unsafeAlgebra)								\
	macro(NoNaNs, noNaNs)															\
	macro(NoInfs, noInfs)															\
	macro(NoSignedZeros, noSignedZeros)								\
	macro(AllowReciprocal, allowReciprocal)

typedef enum {
#define ENUM_CASE(x,l) LLVM ## x ## Bit,
LLVM_GENERAL_FOR_EACH_FAST_MATH_FLAG(ENUM_CASE)
#undef ENUM_CASE
} LLVMFastMathFlagBit;

typedef enum {
#define ENUM_CASE(x,l) LLVM ## x = (1 << LLVM ## x ## Bit),
LLVM_GENERAL_FOR_EACH_FAST_MATH_FLAG(ENUM_CASE)
#undef ENUM_CASE
} LLVMFastMathFlags;

#endif
