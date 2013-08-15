#define __STDC_LIMIT_MACROS
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"

#include "llvm-c/Core.h"

#include "LLVM/General/Internal/FFI/Instruction.h"

using namespace llvm;

namespace llvm {
static AtomicOrdering unwrap(LLVMAtomicOrdering l) {
	switch(l) {
#define ENUM_CASE(x) case LLVMAtomicOrdering ## x: return x;
LLVM_GENERAL_FOR_EACH_ATOMIC_ORDERING(ENUM_CASE)
#undef ENUM_CASE
	default: return AtomicOrdering(0);
	}
}

static SynchronizationScope unwrap(LLVMSynchronizationScope l) {
	switch(l) {
#define ENUM_CASE(x) case LLVM ## x ## SynchronizationScope: return x;
LLVM_GENERAL_FOR_EACH_SYNCRONIZATION_SCOPE(ENUM_CASE)
#undef ENUM_CASE
	default: return SynchronizationScope(0);
	}
}


static AtomicRMWInst::BinOp unwrap(LLVMAtomicRMWBinOp l) {
	switch(l) {
#define ENUM_CASE(x) case LLVMAtomicRMWBinOp ## x: return AtomicRMWInst::x;
LLVM_GENERAL_FOR_EACH_RMW_OPERATION(ENUM_CASE)
#undef ENUM_CASE
	default: return AtomicRMWInst::BinOp(0);
	}
}

}

extern "C" {

#define LLVM_GENERAL_FOR_ALL_OVERFLOWING_BINARY_OPERATORS(macro) \
	macro(Add) \
	macro(Mul) \
	macro(Shl) \
	macro(Sub) \

#define ENUM_CASE(Op)																										\
LLVMValueRef LLVM_General_Build ## Op(																	\
	LLVMBuilderRef b,																											\
	LLVMBool nsw,																													\
	LLVMBool nuw,																													\
	LLVMValueRef o0,																											\
	LLVMValueRef o1,																											\
	const char *s																													\
) {																																			\
	return wrap(unwrap(b)->Create ## Op(unwrap(o0), unwrap(o1), s, nuw, nsw)); \
}
LLVM_GENERAL_FOR_ALL_OVERFLOWING_BINARY_OPERATORS(ENUM_CASE)
#undef ENUM_CASE

#define LLVM_GENERAL_FOR_ALL_POSSIBLY_EXACT_OPERATORS(macro) \
	macro(AShr) \
	macro(LShr) \
	macro(SDiv) \
	macro(UDiv) \

#define ENUM_CASE(Op)																										\
LLVMValueRef LLVM_General_Build ## Op(																	\
	LLVMBuilderRef b,																											\
	LLVMBool exact,																												\
	LLVMValueRef o0,																											\
	LLVMValueRef o1,																											\
	const char *s																													\
) {																																			\
	return wrap(unwrap(b)->Create ## Op(unwrap(o0), unwrap(o1), s, exact)); \
}
LLVM_GENERAL_FOR_ALL_POSSIBLY_EXACT_OPERATORS(ENUM_CASE)
#undef ENUM_CASE


LLVMValueRef LLVM_General_BuildLoad(
	LLVMBuilderRef b,
	LLVMValueRef p,
	unsigned align,
	LLVMBool isVolatile,
	LLVMAtomicOrdering atomicOrdering,
	LLVMSynchronizationScope synchScope,
	const char *name
) {
	LoadInst *i = unwrap(b)->CreateAlignedLoad(unwrap(p), align, isVolatile, name);
	i->setOrdering(unwrap(atomicOrdering));
	if (atomicOrdering != LLVMAtomicOrderingNotAtomic) i->setSynchScope(unwrap(synchScope));
	return wrap(i);
}

LLVMValueRef LLVM_General_BuildStore(
	LLVMBuilderRef b,
	LLVMValueRef v,
	LLVMValueRef p,
	unsigned align,
	LLVMBool isVolatile,
	LLVMAtomicOrdering atomicOrdering,
	LLVMSynchronizationScope synchScope,
	const char *name
) {
	StoreInst *i = unwrap(b)->CreateAlignedStore(unwrap(v), unwrap(p), align, isVolatile);
	i->setName(name);
	i->setOrdering(unwrap(atomicOrdering));
	if (atomicOrdering != LLVMAtomicOrderingNotAtomic) i->setSynchScope(unwrap(synchScope));
	return wrap(i);
}

LLVMValueRef LLVMBuildFence(
	LLVMBuilderRef b, LLVMAtomicOrdering lao, LLVMSynchronizationScope lss, const char *name
) {
	FenceInst *i = unwrap(b)->CreateFence(unwrap(lao), unwrap(lss));
	i->setName(name);
	return wrap(i);
}

LLVMValueRef LLVM_General_BuildAtomicCmpXchg(
	LLVMBuilderRef b,
	LLVMValueRef ptr, 
	LLVMValueRef cmp, 
	LLVMValueRef n, 
	LLVMBool v,
	LLVMAtomicOrdering lao,
	LLVMSynchronizationScope lss,
	const char *name
) {
	AtomicCmpXchgInst *a = unwrap(b)->CreateAtomicCmpXchg(
		unwrap(ptr), unwrap(cmp), unwrap(n), unwrap(lao), unwrap(lss)
	);
	a->setVolatile(v);
	a->setName(name);
	return wrap(a);
}

LLVMValueRef LLVM_General_BuildAtomicRMW(
	LLVMBuilderRef b,
	LLVMAtomicRMWBinOp rmwOp,
	LLVMValueRef ptr, 
	LLVMValueRef val, 
	LLVMBool v,
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

LLVMValueRef LLVM_General_BuildExtractValue(
	LLVMBuilderRef b,
	LLVMValueRef a,
	unsigned *idxs,
	unsigned n,
	const char *name
) {
	return wrap(unwrap(b)->CreateExtractValue(unwrap(a), ArrayRef<unsigned>(idxs, n), name));
}

LLVMValueRef LLVM_General_BuildInsertValue(
	LLVMBuilderRef b,
	LLVMValueRef a,
	LLVMValueRef v,
	unsigned *idxs,
	unsigned n,
	const char *name
) {
	return wrap(unwrap(b)->CreateInsertValue(unwrap(a), unwrap(v), ArrayRef<unsigned>(idxs, n), name));
}

}
