#define __STDC_LIMIT_MACROS
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"

#include "llvm-c/Core.h"

#include "LLVM/Internal/FFI/Instruction.h"

using namespace llvm;

namespace llvm {
static AtomicOrdering unwrap(LLVMAtomicOrdering l) {
    switch(l) {
        case LLVMAtomicOrderingNotAtomic: return AtomicOrdering::NotAtomic;
        case LLVMAtomicOrderingUnordered: return AtomicOrdering::Unordered;
        case LLVMAtomicOrderingMonotonic: return AtomicOrdering::Monotonic;
        case LLVMAtomicOrderingAcquire: return AtomicOrdering::Acquire;
        case LLVMAtomicOrderingRelease: return AtomicOrdering::Release;
        case LLVMAtomicOrderingAcquireRelease:  return AtomicOrdering::AcquireRelease;
        case LLVMAtomicOrderingSequentiallyConsistent: return AtomicOrdering::SequentiallyConsistent;
        //FIXME: this function should be total and no default should be required
        default: return AtomicOrdering(0);
    }
}

static SyncScope::ID unwrap(LLVMSynchronizationScope l) {
    switch(l) {
        case LLVMSingleThreadSynchronizationScope: return SyncScope::SingleThread;
        case LLVMSystemSynchronizationScope: return SyncScope::System;
        //FIXME: this function should be total and no default should be required
        default: return SyncScope::ID(0);
    }
}

static AtomicRMWInst::BinOp unwrap(LLVMAtomicRMWBinOp_ l) {
    switch(l) {
#define ENUM_CASE(x) case LLVMAtomicRMWBinOp_ ## x: return AtomicRMWInst::x;
LLVM_HS_FOR_EACH_RMW_OPERATION(ENUM_CASE)
#undef ENUM_CASE
    //FIXME: this function should be total and no default should be required
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

LLVMValueRef LLVM_Hs_BuildAdd(
    LLVMBuilderRef b,
    LLVMBool nsw,
    LLVMBool nuw,
    LLVMValueRef o0,
    LLVMValueRef o1,
    const char *s
) {
    if (nuw) {
        return LLVMBuildNUWAdd(b, o0, o1, s);
    } else if (nsw) {
        return LLVMBuildNSWAdd(b, o0, o1, s);
    } else {
        return LLVMBuildAdd(b, o0, o1, s);
    }
}

LLVMValueRef LLVM_Hs_BuildMul(
    LLVMBuilderRef b,
    LLVMBool nsw,
    LLVMBool nuw,
    LLVMValueRef o0,
    LLVMValueRef o1,
    const char *s
) {
    if (nuw) {
        return LLVMBuildNUWMul(b, o0, o1, s);
    } else if (nsw) {
        return LLVMBuildNSWMul(b, o0, o1, s);
    } else {
        return LLVMBuildMul(b, o0, o1, s);
    }
}

LLVMValueRef LLVM_Hs_BuildShl(
    LLVMBuilderRef b,
    LLVMBool nsw,
    LLVMBool nuw,
    LLVMValueRef o0,
    LLVMValueRef o1,
    const char *s
) {
    // TODO-LLVM-VERSION-INCREASE: Check C API coverage (has this functionality been added?)
    // For some reason, the LLVM C API does not provide NUW/NSW constructors for Shl
    BinaryOperator *BO = unwrap(b)->Insert(BinaryOperator::Create(Instruction::Shl, unwrap(o0), unwrap(o1)), s);
    if (nuw) BO->setHasNoUnsignedWrap();
    if (nsw) BO->setHasNoSignedWrap();
    return wrap(BO);
}

LLVMValueRef LLVM_Hs_BuildSub(
    LLVMBuilderRef b,
    LLVMBool nsw,
    LLVMBool nuw,
    LLVMValueRef o0,
    LLVMValueRef o1,
    const char *s
) {
    if (nuw) {
        return LLVMBuildNUWSub(b, o0, o1, s);
    } else if (nsw) {
        return LLVMBuildNSWSub(b, o0, o1, s);
    } else {
        return LLVMBuildSub(b, o0, o1, s);
    }
}

LLVMValueRef LLVM_Hs_BuildUDiv(
    LLVMBuilderRef b,
    LLVMBool exact,
    LLVMValueRef o0,
    LLVMValueRef o1,
    const char *s
) {
    if (!exact) {
        return wrap(unwrap(b)->Insert(BinaryOperator::CreateUDiv(unwrap(o0), unwrap(o1)), s));
    }
    return wrap(unwrap(b)->Insert(BinaryOperator::CreateExactUDiv(unwrap(o0), unwrap(o1)), s));
}

LLVMValueRef LLVM_Hs_BuildSDiv(
    LLVMBuilderRef b,
    LLVMBool exact,
    LLVMValueRef o0,
    LLVMValueRef o1,
    const char *s
) {
    if (!exact) {
        return wrap(unwrap(b)->Insert(BinaryOperator::CreateSDiv(unwrap(o0), unwrap(o1)), s));
    }
    return wrap(unwrap(b)->Insert(BinaryOperator::CreateExactSDiv(unwrap(o0), unwrap(o1)), s));
}

LLVMValueRef LLVM_Hs_BuildLShr(
    LLVMBuilderRef b,
    LLVMBool exact,
    LLVMValueRef o0,
    LLVMValueRef o1,
    const char *s
) {
    if (!exact) {
        return wrap(unwrap(b)->Insert(BinaryOperator::CreateLShr(unwrap(o0), unwrap(o1)), s));
    }
    return wrap(unwrap(b)->Insert(BinaryOperator::CreateExactLShr(unwrap(o0), unwrap(o1)), s));
}

LLVMValueRef LLVM_Hs_BuildAShr(
    LLVMBuilderRef b,
    LLVMBool exact,
    LLVMValueRef o0,
    LLVMValueRef o1,
    const char *s
) {
    if (!exact) {
        return wrap(unwrap(b)->Insert(BinaryOperator::CreateAShr(unwrap(o0), unwrap(o1)), s));
    }
    return wrap(unwrap(b)->Insert(BinaryOperator::CreateExactAShr(unwrap(o0), unwrap(o1)), s));
}

LLVMValueRef LLVM_Hs_BuildAddrSpaceCast(LLVMBuilderRef b, LLVMValueRef val, LLVMTypeRef destTy, const char *name) {
    return wrap(unwrap(b)->Insert(CastInst::Create(Instruction::AddrSpaceCast, unwrap(val), unwrap(destTy)), name));
}

LLVMValueRef LLVM_Hs_BuildBitCast(LLVMBuilderRef b, LLVMValueRef val, LLVMTypeRef destTy, const char *name) {
    return wrap(unwrap(b)->Insert(CastInst::Create(Instruction::BitCast, unwrap(val), unwrap(destTy)), name));
}

LLVMValueRef LLVM_Hs_BuildFPExt(LLVMBuilderRef b, LLVMValueRef val, LLVMTypeRef destTy, const char *name) {
    return wrap(unwrap(b)->Insert(CastInst::Create(Instruction::FPExt, unwrap(val), unwrap(destTy)), name));
}

LLVMValueRef LLVM_Hs_BuildFPToSI(LLVMBuilderRef b, LLVMValueRef val, LLVMTypeRef destTy, const char *name) {
    return wrap(unwrap(b)->Insert(CastInst::Create(Instruction::FPToSI, unwrap(val), unwrap(destTy)), name));
}

LLVMValueRef LLVM_Hs_BuildFPToUI(LLVMBuilderRef b, LLVMValueRef val, LLVMTypeRef destTy, const char *name) {
    return wrap(unwrap(b)->Insert(CastInst::Create(Instruction::FPToUI, unwrap(val), unwrap(destTy)), name));
}

LLVMValueRef LLVM_Hs_BuildFPTrunc(LLVMBuilderRef b, LLVMValueRef val, LLVMTypeRef destTy, const char *name) {
    return wrap(unwrap(b)->Insert(CastInst::Create(Instruction::FPTrunc, unwrap(val), unwrap(destTy)), name));
}

LLVMValueRef LLVM_Hs_BuildIntToPtr(LLVMBuilderRef b, LLVMValueRef val, LLVMTypeRef destTy, const char *name) {
    return wrap(unwrap(b)->Insert(CastInst::Create(Instruction::IntToPtr, unwrap(val), unwrap(destTy)), name));
}

LLVMValueRef LLVM_Hs_BuildPtrToInt(LLVMBuilderRef b, LLVMValueRef val, LLVMTypeRef destTy, const char *name) {
    return wrap(unwrap(b)->Insert(CastInst::Create(Instruction::PtrToInt, unwrap(val), unwrap(destTy)), name));
}

LLVMValueRef LLVM_Hs_BuildSExt(LLVMBuilderRef b, LLVMValueRef val, LLVMTypeRef destTy, const char *name) {
    return wrap(unwrap(b)->Insert(CastInst::Create(Instruction::SExt, unwrap(val), unwrap(destTy)), name));
}

LLVMValueRef LLVM_Hs_BuildSIToFP(LLVMBuilderRef b, LLVMValueRef val, LLVMTypeRef destTy, const char *name) {
    return wrap(unwrap(b)->Insert(CastInst::Create(Instruction::SIToFP, unwrap(val), unwrap(destTy)), name));
}

LLVMValueRef LLVM_Hs_BuildTrunc(LLVMBuilderRef b, LLVMValueRef val, LLVMTypeRef destTy, const char *name) {
    return wrap(unwrap(b)->Insert(CastInst::Create(Instruction::Trunc, unwrap(val), unwrap(destTy)), name));
}

LLVMValueRef LLVM_Hs_BuildUIToFP(LLVMBuilderRef b, LLVMValueRef val, LLVMTypeRef destTy, const char *name) {
    return wrap(unwrap(b)->Insert(CastInst::Create(Instruction::UIToFP, unwrap(val), unwrap(destTy)), name));
}

LLVMValueRef LLVM_Hs_BuildZExt(LLVMBuilderRef b, LLVMValueRef val, LLVMTypeRef destTy, const char *name) {
    return wrap(unwrap(b)->Insert(CastInst::Create(Instruction::ZExt, unwrap(val), unwrap(destTy)), name));
}

LLVMValueRef LLVM_Hs_BuildAnd(LLVMBuilderRef b, LLVMValueRef lhs, LLVMValueRef rhs, const char *name) {
    return wrap(unwrap(b)->Insert(BinaryOperator::CreateAnd(unwrap(lhs), unwrap(rhs)), name));
}

LLVMValueRef LLVM_Hs_BuildOr(LLVMBuilderRef b, LLVMValueRef lhs, LLVMValueRef rhs, const char *name) {
    return wrap(unwrap(b)->Insert(BinaryOperator::CreateOr(unwrap(lhs), unwrap(rhs)), name));
}

LLVMValueRef LLVM_Hs_BuildSRem(LLVMBuilderRef b, LLVMValueRef lhs, LLVMValueRef rhs, const char *name) {
    return wrap(unwrap(b)->Insert(BinaryOperator::CreateSRem(unwrap(lhs), unwrap(rhs)), name));
}

LLVMValueRef LLVM_Hs_BuildURem(LLVMBuilderRef b, LLVMValueRef lhs, LLVMValueRef rhs, const char *name) {
    return wrap(unwrap(b)->Insert(BinaryOperator::CreateURem(unwrap(lhs), unwrap(rhs)), name));
}

LLVMValueRef LLVM_Hs_BuildXor(LLVMBuilderRef b, LLVMValueRef lhs, LLVMValueRef rhs, const char *name) {
    return wrap(unwrap(b)->Insert(BinaryOperator::CreateXor(unwrap(lhs), unwrap(rhs)), name));
}

LLVMValueRef LLVM_Hs_BuildFNeg(LLVMBuilderRef b, LLVMValueRef rhs, const char *name) {
    return LLVMBuildFNeg(b, rhs, name);
}

LLVMValueRef LLVM_Hs_BuildFAdd(LLVMBuilderRef b, LLVMValueRef lhs, LLVMValueRef rhs, const char *name) {
    BinaryOperator* bo = BinaryOperator::CreateFAdd(unwrap(lhs), unwrap(rhs));
    bo->setFastMathFlags(unwrap(b)->getFastMathFlags());
    return wrap(unwrap(b)->Insert(bo, name));
}

LLVMValueRef LLVM_Hs_BuildFDiv(LLVMBuilderRef b, LLVMValueRef lhs, LLVMValueRef rhs, const char *name) {
    BinaryOperator* bo = BinaryOperator::CreateFDiv(unwrap(lhs), unwrap(rhs));
    bo->setFastMathFlags(unwrap(b)->getFastMathFlags());
    return wrap(unwrap(b)->Insert(bo, name));
}

LLVMValueRef LLVM_Hs_BuildFMul(LLVMBuilderRef b, LLVMValueRef lhs, LLVMValueRef rhs, const char *name) {
    BinaryOperator* bo = BinaryOperator::CreateFMul(unwrap(lhs), unwrap(rhs));
    bo->setFastMathFlags(unwrap(b)->getFastMathFlags());
    return wrap(unwrap(b)->Insert(bo, name));
}

LLVMValueRef LLVM_Hs_BuildFRem(LLVMBuilderRef b, LLVMValueRef lhs, LLVMValueRef rhs, const char *name) {
    BinaryOperator* bo = BinaryOperator::CreateFRem(unwrap(lhs), unwrap(rhs));
    bo->setFastMathFlags(unwrap(b)->getFastMathFlags());
    return wrap(unwrap(b)->Insert(bo, name));
}

LLVMValueRef LLVM_Hs_BuildFSub(LLVMBuilderRef b, LLVMValueRef lhs, LLVMValueRef rhs, const char *name) {
    BinaryOperator* bo = BinaryOperator::CreateFSub(unwrap(lhs), unwrap(rhs));
    bo->setFastMathFlags(unwrap(b)->getFastMathFlags());
    return wrap(unwrap(b)->Insert(bo, name));
}

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
    LoadInst *i = unwrap(b)->CreateAlignedLoad(unwrap(p), MaybeAlign(align), isVolatile, name);
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
    StoreInst *i = unwrap(b)->CreateAlignedStore(unwrap(v), unwrap(p), MaybeAlign(align), isVolatile);
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
    LLVMAtomicRMWBinOp_ rmwOp,
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
                                        LLVMValueRef V2, int *MaskArgs,
                                        unsigned MaskSize, const char *Name) {
    ArrayRef<int> maskArray(MaskArgs, MaskSize);
    return wrap(unwrap(B)->Insert(new ShuffleVectorInst(unwrap(V1), unwrap(V2),
                                                        maskArray),
                                  Name));
}
}
