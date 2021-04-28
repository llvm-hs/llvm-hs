#ifndef __LLVM_INTERNAL_FFI__INSTRUCTION__H__
#define __LLVM_INTERNAL_FFI__INSTRUCTION__H__

#include "llvm/Config/llvm-config.h"

#define LLVM_HS_FOR_EACH_ATOMIC_ORDERING(macro) \
	macro(NotAtomic) \
	macro(Unordered) \
	macro(Monotonic) \
	macro(Acquire) \
	macro(Release) \
	macro(AcquireRelease) \
	macro(SequentiallyConsistent)

#define LLVM_HS_FOR_EACH_RMW_OPERATION(macro) \
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

#define LLVM_HS_FOR_EACH_SYNCRONIZATION_SCOPE(macro) \
	macro(SingleThread) \
	macro(System)

typedef enum {
#define ENUM_CASE(x) LLVM ## x ## SynchronizationScope,
LLVM_HS_FOR_EACH_SYNCRONIZATION_SCOPE(ENUM_CASE)
#undef ENUM_CASE
} LLVMSynchronizationScope;

/* The last parameter to the macro indicates whether the set<param> function takes a boolean argument or not */
#define LLVM_HS_FOR_EACH_FAST_MATH_FLAG(macro) \
	macro(AllowReassoc, allowReassoc, F)								\
	macro(NoNaNs, noNaNs, F)                                              \
	macro(NoInfs, noInfs, F)                                              \
	macro(NoSignedZeros, noSignedZeros, F)								\
	macro(AllowReciprocal, allowReciprocal, F)                            \
    macro(AllowContract, allowContract, T) \
    macro(ApproxFunc, approxFunc, F)

typedef enum {
#define ENUM_CASE(x,l,takesArg) LLVM ## x ## Bit,
LLVM_HS_FOR_EACH_FAST_MATH_FLAG(ENUM_CASE)
#undef ENUM_CASE
} LLVMFastMathFlagBit;

typedef enum {
#define ENUM_CASE(x,l,takesArg) LLVM ## x = (1 << LLVM ## x ## Bit),
LLVM_HS_FOR_EACH_FAST_MATH_FLAG(ENUM_CASE)
#undef ENUM_CASE
} LLVMFastMathFlags;

#define LLVM_HS_FOR_EACH_TAIL_CALL_KIND(macro) \
	macro(None)                                       \
	macro(Tail)                                       \
	macro(MustTail)                                   \
	macro(NoTail)

typedef enum {
#define ENUM_CASE(x) LLVM_Hs_TailCallKind_ ## x,
LLVM_HS_FOR_EACH_TAIL_CALL_KIND(ENUM_CASE)
#undef ENUM_CASE
} LLVM_Hs_TailCallKind;
#endif
