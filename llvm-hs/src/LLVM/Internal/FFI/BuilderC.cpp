#define __STDC_LIMIT_MACROS
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"

#include "llvm-c/Core.h"

#include "LLVM/Internal/FFI/BinaryOperator.h"
#include "LLVM/Internal/FFI/ErrorHandling.hpp"
#include "LLVM/Internal/FFI/Instruction.h"

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

static SyncScope::ID unwrap(LLVMSynchronizationScope l) {
	switch(l) {
#define ENUM_CASE(x) case LLVM ## x ## SynchronizationScope: return SyncScope::x;
LLVM_HS_FOR_EACH_SYNCRONIZATION_SCOPE(ENUM_CASE)
#undef ENUM_CASE
	default: reportFatalError("Unknown synchronization scope");
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
#define ENUM_CASE_F(x,l) if (f & LLVM ## x) r.set ## x();
#define ENUM_CASE_T(x,l) if (f & LLVM ## x) r.set ## x(true);
#define ENUM_CASE(x,l,takesArg) ENUM_CASE_ ## takesArg(x,l)
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
    BinaryOperator *BO = unwrap(b)->Insert(BinaryOperator::Create(Instruction::Op, unwrap(o0), unwrap(o1)), s); \
    if (nuw) BO->setHasNoUnsignedWrap(); \
    if (nsw) BO->setHasNoSignedWrap(); \
    return wrap(BO);                   \
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
    if (!exact) { \
        return wrap(unwrap(b)->Insert(BinaryOperator::Create##Op(unwrap(o0), unwrap(o1)), s)); \
    } \
     return wrap(unwrap(b)->Insert(BinaryOperator::CreateExact##Op(unwrap(o0), unwrap(o1)), s)); \
}
LLVM_HS_FOR_EACH_POSSIBLY_EXACT_BINARY_OPERATOR(ENUM_CASE)
#undef ENUM_CASE

#define LLVM_HS_FOR_EACH_CAST_INSTR(macro) \
    macro(AddrSpaceCast) \
    macro(BitCast) \
    macro(FPExt) \
    macro(FPToSI) \
    macro(FPToUI) \
    macro(FPTrunc) \
    macro(IntToPtr) \
    macro(PtrToInt) \
    macro(SExt) \
    macro(SIToFP) \
    macro(Trunc) \
    macro(UIToFP) \
    macro(ZExt)

#define ENUM_CASE(Op) \
LLVMValueRef LLVM_Hs_Build##Op(LLVMBuilderRef b, LLVMValueRef val, LLVMTypeRef destTy, const char *name) {\
    return wrap(unwrap(b)->Insert(CastInst::Create(Instruction::Op, unwrap(val), unwrap(destTy)), name));\
}
LLVM_HS_FOR_EACH_CAST_INSTR(ENUM_CASE)
#undef ENUM_CASE

#define LLVM_HS_FOR_EACH_BINOP(macro) \
    macro(And) \
    macro(Or) \
    macro(SRem) \
    macro(URem) \
    macro(Xor)

#define ENUM_CASE(Op) \
LLVMValueRef LLVM_Hs_Build##Op(LLVMBuilderRef b, LLVMValueRef lhs, LLVMValueRef rhs, const char *name) {\
    return wrap(unwrap(b)->Insert(BinaryOperator::Create##Op(unwrap(lhs), unwrap(rhs)), name)); \
}
LLVM_HS_FOR_EACH_BINOP(ENUM_CASE)
#undef ENUM_CASE

#define LLVM_HS_FOR_EACH_FP_BINOP(macro) \
    macro(FAdd) \
    macro(FDiv) \
    macro(FMul) \
    macro(FRem) \
    macro(FSub)

#define ENUM_CASE(Op) \
LLVMValueRef LLVM_Hs_Build##Op(LLVMBuilderRef b, LLVMValueRef lhs, LLVMValueRef rhs, const char *name) { \
    BinaryOperator* bo = BinaryOperator::Create##Op(unwrap(lhs), unwrap(rhs)); \
    bo->setFastMathFlags(unwrap(b)->getFastMathFlags()); \
    return wrap(unwrap(b)->Insert(bo, name)); \
}
LLVM_HS_FOR_EACH_FP_BINOP(ENUM_CASE)
#undef ENUM_CASE

void LLVM_Hs_SetFastMathFlags(LLVMBuilderRef b, LLVMFastMathFlags f) {
	unwrap(b)->setFastMathFlags(unwrap(f));
}

LLVMValueRef LLVM_Hs_BuildICmp(LLVMBuilderRef b, LLVMIntPredicate op, LLVMValueRef lhs, LLVMValueRef rhs, const char* name) {

    return wrap(unwrap(b)->Insert(new ICmpInst(static_cast<ICmpInst::Predicate>(op),
                                               unwrap(lhs), unwrap(rhs)), name));
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
	if (atomicOrdering != LLVMAtomicOrderingNotAtomic) i->setSyncScopeID(unwrap(synchScope));
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
	if (atomicOrdering != LLVMAtomicOrderingNotAtomic) i->setSyncScopeID(unwrap(synchScope));
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

LLVMValueRef LLVM_Hs_BuildCleanupPad(LLVMBuilderRef b, LLVMValueRef parentPad,
                                     LLVMValueRef *args, unsigned numArgs,
                                     const char *name) {
  return wrap(unwrap(b)->CreateCleanupPad(unwrap(parentPad),
                                          makeArrayRef(unwrap(args), numArgs),
                                          name));
}

LLVMValueRef LLVM_Hs_BuildCatchPad(LLVMBuilderRef b, LLVMValueRef catchSwitch,
                                   LLVMValueRef *args, unsigned numArgs,
                                   const char *name) {
    return wrap(unwrap(b)->CreateCatchPad(unwrap(catchSwitch),
                                          makeArrayRef(unwrap(args), numArgs),
                                          name));
}

LLVMValueRef LLVM_Hs_BuildCleanupRet(LLVMBuilderRef b, LLVMValueRef cleanupPad,
                                     LLVMBasicBlockRef unwindDest) {
    // Due to the way name resolution works in llvm-hs, cleanupPad might not
    // actually be a CleanupPadInst. However, it will later be replaced by one.
    // Pretending that we have one is thus ok here.
    auto cleanupPad_ = static_cast<CleanupPadInst*>(unwrap<Value>(cleanupPad));
    return wrap(unwrap(b)->CreateCleanupRet(cleanupPad_,
                                            unwrap(unwindDest)));
}

LLVMValueRef LLVM_Hs_BuildCatchRet(LLVMBuilderRef b, LLVMValueRef catchPad,
                                   LLVMBasicBlockRef successor) {
    // Due to the way name resolution works in llvm-hs, catchPad might not
    // actually be a CatchPadInst. However, it will later be replaced by one.
    // Pretending that we have one is thus ok here.
    auto catchPad_ = static_cast<CatchPadInst *>(unwrap<Value>(catchPad));
    return wrap(unwrap(b)->CreateCatchRet(catchPad_, unwrap(successor)));
}

LLVMValueRef LLVM_Hs_BuildCatchSwitch(LLVMBuilderRef b, LLVMValueRef parentPad,
                                      LLVMBasicBlockRef unwindDest,
                                      unsigned numHandlers) {
    return wrap(unwrap(b)->CreateCatchSwitch(unwrap(parentPad),
                                             unwrap(unwindDest), numHandlers));
}

LLVMValueRef LLVM_Hs_BuildGEP(LLVMBuilderRef B, LLVMValueRef Pointer,
                             LLVMValueRef *Indices, unsigned NumIndices,
                             const char *Name) {
  ArrayRef<Value *> IdxList(unwrap(Indices), NumIndices);
  return wrap(unwrap(B)->Insert(GetElementPtrInst::Create(nullptr, unwrap(Pointer), IdxList), Name));
}

LLVMValueRef LLVM_Hs_BuildInBoundsGEP(LLVMBuilderRef B, LLVMValueRef Pointer,
                                      LLVMValueRef *Indices, unsigned NumIndices,
                                      const char *Name) {
  ArrayRef<Value *> IdxList(unwrap(Indices), NumIndices);
  return wrap(unwrap(B)->Insert(GetElementPtrInst::CreateInBounds(nullptr, unwrap(Pointer), IdxList), Name));
}

LLVMValueRef LLVM_Hs_BuildSelect(LLVMBuilderRef B, LLVMValueRef If,
                                 LLVMValueRef Then, LLVMValueRef Else,
                                 const char *Name) {
    return wrap(unwrap(B)->Insert(SelectInst::Create(unwrap(If), unwrap(Then), unwrap(Else))));
}

LLVMValueRef LLVM_Hs_BuildExtractValue(
	LLVMBuilderRef b,
	LLVMValueRef a,
	unsigned *idxs,
	unsigned n,
	const char *name
) {
	return wrap(unwrap(b)->Insert(ExtractValueInst::Create(unwrap(a), ArrayRef<unsigned>(idxs, n)), name));
}

LLVMValueRef LLVM_Hs_BuildInsertValue(
	LLVMBuilderRef b,
	LLVMValueRef a,
	LLVMValueRef v,
	unsigned *idxs,
	unsigned n,
	const char *name
) {
	return wrap(unwrap(b)->Insert(InsertValueInst::Create(unwrap(a), unwrap(v), ArrayRef<unsigned>(idxs, n)), name));
}

LLVMValueRef LLVM_Hs_BuildExtractElement(LLVMBuilderRef B, LLVMValueRef VecVal,
                                         LLVMValueRef Index, const char *Name) {
    return wrap(unwrap(B)->Insert(ExtractElementInst::Create(unwrap(VecVal), unwrap(Index)),
                                  Name));
}

LLVMValueRef LLVM_Hs_BuildInsertElement(LLVMBuilderRef B, LLVMValueRef VecVal,
                                        LLVMValueRef EltVal, LLVMValueRef Index,
                                        const char *Name) {
    return wrap(unwrap(B)->Insert(InsertElementInst::Create(unwrap(VecVal), unwrap(EltVal),
                                                            unwrap(Index)),
                                  Name));
}

LLVMValueRef LLVM_Hs_BuildShuffleVector(LLVMBuilderRef B, LLVMValueRef V1,
                                        LLVMValueRef V2, LLVMValueRef Mask,
                                        const char *Name) {
    return wrap(unwrap(B)->Insert(new ShuffleVectorInst(unwrap(V1), unwrap(V2),
                                                        unwrap(Mask)),
                                  Name));
}
}
