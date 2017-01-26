#define __STDC_LIMIT_MACROS
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"

#include "llvm-c/Core.h"

#include "LLVM/Internal/FFI/Instruction.h"
#include "LLVM/Internal/FFI/BinaryOperator.h"

using namespace llvm;

namespace llvm {
static AtomicOrdering unwrap(LLVMAtomicOrdering l) {
	switch(l) {
#define ENUM_CASE(x) case LLVMAtomicOrdering ## x: return AtomicOrdering::x;
LLVM_HS_FOR_EACH_ATOMIC_ORDERING(ENUM_CASE)
#undef ENUM_CASE
	default: return AtomicOrdering(0);
	}
}

static SynchronizationScope unwrap(LLVMSynchronizationScope l) {
	switch(l) {
#define ENUM_CASE(x) case LLVM ## x ## SynchronizationScope: return x;
LLVM_HS_FOR_EACH_SYNCRONIZATION_SCOPE(ENUM_CASE)
#undef ENUM_CASE
	default: return SynchronizationScope(0);
	}
}


static AtomicRMWInst::BinOp unwrap(LLVMAtomicRMWBinOp l) {
	switch(l) {
#define ENUM_CASE(x) case LLVMAtomicRMWBinOp ## x: return AtomicRMWInst::x;
LLVM_HS_FOR_EACH_RMW_OPERATION(ENUM_CASE)
#undef ENUM_CASE
	default: return AtomicRMWInst::BinOp(0);
	}
}

static FastMathFlags unwrap(LLVMFastMathFlags f) {
	FastMathFlags r = FastMathFlags();
#define ENUM_CASE(x,l) if (f & LLVM ## x) r.set ## x();
LLVM_HS_FOR_EACH_FAST_MATH_FLAG(ENUM_CASE)
#undef ENUM_CASE
	return r;
}

}

extern "C" {

#define ENUM_CASE(Op)																										\
LLVMValueRef LLVM_Hs_Build ## Op(																	\
	LLVMBuilderRef b,																											\
	LLVMBool nsw,																													\
	LLVMBool nuw,																													\
	LLVMValueRef o0,																											\
	LLVMValueRef o1,																											\
	const char *s																													\
) {																																			\
	return wrap(unwrap(b)->Create ## Op(unwrap(o0), unwrap(o1), s, nuw, nsw)); \
}
LLVM_HS_FOR_EACH_OVERFLOWING_BINARY_OPERATOR(ENUM_CASE)
#undef ENUM_CASE

#define ENUM_CASE(Op)																										\
LLVMValueRef LLVM_Hs_Build ## Op(																	\
	LLVMBuilderRef b,																											\
	LLVMBool exact,																												\
	LLVMValueRef o0,																											\
	LLVMValueRef o1,																											\
	const char *s																													\
) {																																			\
	return wrap(unwrap(b)->Create ## Op(unwrap(o0), unwrap(o1), s, exact)); \
}
LLVM_HS_FOR_EACH_POSSIBLY_EXACT_BINARY_OPERATOR(ENUM_CASE)
#undef ENUM_CASE

void LLVM_Hs_SetFastMathFlags(LLVMBuilderRef b, LLVMFastMathFlags f) {
	unwrap(b)->setFastMathFlags(unwrap(f));
}

LLVMValueRef LLVM_Hs_BuildLoad(
	LLVMBuilderRef b,
	LLVMBool isVolatile,
	LLVMValueRef p,
	LLVMAtomicOrdering atomicOrdering,
	LLVMSynchronizationScope synchScope,
	unsigned align,
	const char *name
) {
	LoadInst *i = unwrap(b)->CreateAlignedLoad(unwrap(p), align, isVolatile, name);
	i->setOrdering(unwrap(atomicOrdering));
	if (atomicOrdering != LLVMAtomicOrderingNotAtomic) i->setSynchScope(unwrap(synchScope));
	return wrap(i);
}

LLVMValueRef LLVM_Hs_BuildStore(
	LLVMBuilderRef b,
	LLVMBool isVolatile,
	LLVMValueRef p,
	LLVMValueRef v,
	LLVMAtomicOrdering atomicOrdering,
	LLVMSynchronizationScope synchScope,
	unsigned align,
	const char *name
) {
	StoreInst *i = unwrap(b)->CreateAlignedStore(unwrap(v), unwrap(p), align, isVolatile);
	i->setName(name);
	i->setOrdering(unwrap(atomicOrdering));
	if (atomicOrdering != LLVMAtomicOrderingNotAtomic) i->setSynchScope(unwrap(synchScope));
	return wrap(i);
}

LLVMValueRef LLVM_Hs_BuildFence(
	LLVMBuilderRef b, LLVMAtomicOrdering lao, LLVMSynchronizationScope lss, const char *name
) {
	FenceInst *i = unwrap(b)->CreateFence(unwrap(lao), unwrap(lss));
	i->setName(name);
	return wrap(i);
}

LLVMValueRef LLVM_Hs_BuildAtomicCmpXchg(
	LLVMBuilderRef b,
	LLVMBool v,
	LLVMValueRef ptr, 
	LLVMValueRef cmp, 
	LLVMValueRef n, 
	LLVMAtomicOrdering successOrdering,
	LLVMAtomicOrdering failureOrdering,
	LLVMSynchronizationScope lss,
	const char *name
) {
	AtomicCmpXchgInst *a = unwrap(b)->CreateAtomicCmpXchg(
		unwrap(ptr), unwrap(cmp), unwrap(n), unwrap(successOrdering), unwrap(failureOrdering), unwrap(lss)
	);
	a->setVolatile(v);
	a->setName(name);
	return wrap(a);
}

LLVMValueRef LLVM_Hs_BuildAtomicRMW(
	LLVMBuilderRef b,
	LLVMBool v,
	LLVMAtomicRMWBinOp rmwOp,
	LLVMValueRef ptr, 
	LLVMValueRef val, 
	LLVMAtomicOrdering lao,
	LLVMSynchronizationScope lss,
	const char *name
) {
	AtomicRMWInst *a = unwrap(b)->CreateAtomicRMW(
		unwrap(rmwOp), unwrap(ptr), unwrap(val), unwrap(lao), unwrap(lss)
	);
	a->setVolatile(v);
	a->setName(name);
	return wrap(a);
}

LLVMValueRef LLVM_Hs_BuildExtractValue(
	LLVMBuilderRef b,
	LLVMValueRef a,
	unsigned *idxs,
	unsigned n,
	const char *name
) {
	return wrap(unwrap(b)->CreateExtractValue(unwrap(a), ArrayRef<unsigned>(idxs, n), name));
}

LLVMValueRef LLVM_Hs_BuildInsertValue(
	LLVMBuilderRef b,
	LLVMValueRef a,
	LLVMValueRef v,
	unsigned *idxs,
	unsigned n,
	const char *name
) {
	return wrap(unwrap(b)->CreateInsertValue(unwrap(a), unwrap(v), ArrayRef<unsigned>(idxs, n), name));
}

LLVMValueRef LLVM_Hs_BuildCleanupPad(LLVMBuilderRef b, LLVMValueRef parentPad,
                                     LLVMValueRef *args, unsigned numArgs,
                                     const char *name) {
  return wrap(unwrap(b)->CreateCleanupPad(unwrap(parentPad),
                                          makeArrayRef(unwrap(args), numArgs),
                                          name));
}

LLVMValueRef LLVM_Hs_BuildCleanupRet(LLVMBuilderRef b, LLVMValueRef cleanupPad,
                                     LLVMBasicBlockRef unwindDest) {
    return wrap(unwrap(b)->CreateCleanupRet(unwrap<CleanupPadInst>(cleanupPad),
                                            unwrap(unwindDest)));
}
}
