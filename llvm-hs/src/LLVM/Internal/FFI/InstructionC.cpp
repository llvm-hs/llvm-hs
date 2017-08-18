#define __STDC_LIMIT_MACROS
#include "llvm/Config/llvm-config.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/Operator.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/CallSite.h"

#include "llvm-c/Core.h"

#include "LLVM/Internal/FFI/AttributeC.hpp"
#include "LLVM/Internal/FFI/ErrorHandling.hpp"
#include "LLVM/Internal/FFI/Instruction.h"

using namespace llvm;

namespace llvm {

static LLVMAtomicOrdering wrap(AtomicOrdering l) {
	switch(l) {
#define ENUM_CASE(x) case AtomicOrdering::x: return LLVMAtomicOrdering ## x;
LLVM_HS_FOR_EACH_ATOMIC_ORDERING(ENUM_CASE)
#undef ENUM_CASE
	default: return LLVMAtomicOrdering(0);
	}
}

static LLVMSynchronizationScope wrap(SyncScope::ID l) {
	switch(l) {
#define ENUM_CASE(x) case SyncScope::x: return LLVM ## x ## SynchronizationScope;
LLVM_HS_FOR_EACH_SYNCRONIZATION_SCOPE(ENUM_CASE)
#undef ENUM_CASE
    default: reportFatalError("Unknown synchronization scope");
	}
}

static LLVMAtomicRMWBinOp wrap(AtomicRMWInst::BinOp l) {
	switch(l) {
#define ENUM_CASE(x) case AtomicRMWInst::x: return LLVMAtomicRMWBinOp ## x;
LLVM_HS_FOR_EACH_RMW_OPERATION(ENUM_CASE)
#undef ENUM_CASE
	default: return LLVMAtomicRMWBinOp(0);
	}
}

LLVMFastMathFlags wrap(FastMathFlags f) {
	unsigned r = 0;
#define ENUM_CASE(u,l,takesArg) if (f.l()) r |= unsigned(LLVM ## u);
LLVM_HS_FOR_EACH_FAST_MATH_FLAG(ENUM_CASE)
#undef ENUM_CASE
    return LLVMFastMathFlags(r);
}

}

extern "C" {

unsigned LLVM_Hs_GetInstructionDefOpcode(LLVMValueRef val) {
	return unwrap<Instruction>(val)->getOpcode();
}

unsigned LLVM_Hs_HasNoSignedWrap(LLVMValueRef val) {
	return unwrap<OverflowingBinaryOperator>(val)->hasNoSignedWrap();
}

unsigned LLVM_Hs_HasNoUnsignedWrap(LLVMValueRef val) {
	return unwrap<OverflowingBinaryOperator>(val)->hasNoUnsignedWrap();
}

int LLVM_Hs_IsExact(LLVMValueRef val) {
	return unwrap<PossiblyExactOperator>(val)->isExact();
}

LLVMFastMathFlags LLVM_Hs_GetFastMathFlags(LLVMValueRef val) {
	return wrap(unwrap<Instruction>(val)->getFastMathFlags());
}

void LLVM_Hs_CallSiteSetAttributeList(LLVMValueRef i, LLVMAttributeListRef attrs) {
	CallSite(unwrap<Instruction>(i)).setAttributes(*attrs);
}

unsigned LLVM_Hs_GetCallSiteCallingConvention(LLVMValueRef i) {
  return unsigned(CallSite(unwrap<Instruction>(i)).getCallingConv());
}

void LLVM_Hs_SetCallSiteCallingConvention(LLVMValueRef i, unsigned cc) {
  CallSite(unwrap<Instruction>(i)).setCallingConv(llvm::CallingConv::ID(cc));
}

LLVMAttributeSetRef LLVM_Hs_CallSiteAttributesAtIndex(LLVMValueRef i, LLVMAttributeIndex idx) {
    auto cs = CallSite(unwrap<Instruction>(i));
    return new AttributeSet(cs.getAttributes().getAttributes(idx));
}

#define CHECK(name)                                                            \
    static_assert(unsigned(llvm::CallInst::TCK_##name) ==                      \
                      unsigned(LLVM_Hs_TailCallKind_##name),              \
                  "LLVM_Hs_TailCallKind enum out of sync w/ "             \
                  "llvm::CallInst::TailCallKind for " #name);
LLVM_HS_FOR_EACH_TAIL_CALL_KIND(CHECK)
#undef CHECK

unsigned LLVM_Hs_GetTailCallKind(LLVMValueRef i) {
    return unwrap<CallInst>(i)->getTailCallKind();
}

void LLVM_Hs_SetTailCallKind(LLVMValueRef i, unsigned kind) {
    return unwrap<CallInst>(i)->setTailCallKind(llvm::CallInst::TailCallKind(kind));
}

LLVMValueRef LLVM_Hs_GetAllocaNumElements(LLVMValueRef a) {
	return wrap(unwrap<AllocaInst>(a)->getArraySize());
}

LLVMTypeRef LLVM_Hs_GetAllocatedType(LLVMValueRef a) {
	return wrap(unwrap<AllocaInst>(a)->getAllocatedType());
}

// ------------------------------------------------------------

#define LLVM_HS_FOR_EACH_ALIGNMENT_INST(macro) \
	macro(Alloca) \
	macro(Load) \
	macro(Store)

unsigned LLVM_Hs_GetInstrAlignment(LLVMValueRef l) {
	switch(unwrap<Instruction>(l)->getOpcode()) {
#define ENUM_CASE(n) case Instruction::n: return unwrap<n ## Inst>(l)->getAlignment();
		LLVM_HS_FOR_EACH_ALIGNMENT_INST(ENUM_CASE)
#undef ENUM_CASE
	default: return 0;
	}
}

void LLVM_Hs_SetInstrAlignment(LLVMValueRef l, unsigned a) {
	switch(unwrap<Instruction>(l)->getOpcode()) {
#define ENUM_CASE(n) case Instruction::n: unwrap<n ## Inst>(l)->setAlignment(a); break;
		LLVM_HS_FOR_EACH_ALIGNMENT_INST(ENUM_CASE)
#undef ENUM_CASE
	}
}

// ------------------------------------------------------------

#define LLVM_HS_FOR_EACH_ATOMIC_INST(macro)	\
	macro(Load,)																		\
	macro(Store,)																		\
	macro(Fence,)																		\
	macro(AtomicCmpXchg,Success)										\
	macro(AtomicRMW,)

LLVMAtomicOrdering LLVM_Hs_GetAtomicOrdering(LLVMValueRef i) {
	switch(unwrap<Instruction>(i)->getOpcode()) {
#define ENUM_CASE(n,s) case Instruction::n: return wrap(unwrap<n ## Inst>(i)->get ## s ## Ordering());
		LLVM_HS_FOR_EACH_ATOMIC_INST(ENUM_CASE)
#undef ENUM_CASE
	default: return LLVMAtomicOrdering(0);
	}
}

LLVMAtomicOrdering LLVM_Hs_GetFailureAtomicOrdering(LLVMValueRef i) {
	switch(unwrap<Instruction>(i)->getOpcode()) {
	case Instruction::AtomicCmpXchg: return wrap(unwrap<AtomicCmpXchgInst>(i)->getFailureOrdering());
	default: return LLVMAtomicOrdering(0);
	}
}

LLVMSynchronizationScope LLVM_Hs_GetSynchronizationScope(LLVMValueRef i) {
	switch(unwrap<Instruction>(i)->getOpcode()) {
#define ENUM_CASE(n,s) case Instruction::n: return wrap(unwrap<n ## Inst>(i)->getSyncScopeID());
		LLVM_HS_FOR_EACH_ATOMIC_INST(ENUM_CASE)
#undef ENUM_CASE
	default: return LLVMSynchronizationScope(0);
	}
}

#define LLVM_HS_FOR_EACH_VOLATILITY_INST(macro) \
	macro(Load) \
	macro(Store) \
	macro(AtomicCmpXchg) \
	macro(AtomicRMW)

LLVMBool LLVM_Hs_GetVolatile(LLVMValueRef i) {
	switch(unwrap<Instruction>(i)->getOpcode()) {
#define ENUM_CASE(n) case Instruction::n: return unwrap<n ## Inst>(i)->isVolatile();
		LLVM_HS_FOR_EACH_VOLATILITY_INST(ENUM_CASE)
#undef ENUM_CASE
	default: return LLVMBool(0);
	}
}

// ------------------------------------------------------------

LLVMBool LLVM_Hs_GetInBounds(LLVMValueRef i) {
	return unwrap<GEPOperator>(i)->isInBounds();
}

LLVMAtomicRMWBinOp LLVM_Hs_GetAtomicRMWBinOp(LLVMValueRef i) {
	return wrap(unwrap<AtomicRMWInst>(i)->getOperation());
}

LLVMRealPredicate LLVM_Hs_GetFCmpPredicate(LLVMValueRef Inst) {
	if (FCmpInst *I = dyn_cast<FCmpInst>(unwrap(Inst)))
		return (LLVMRealPredicate)I->getPredicate();
	if (ConstantExpr *CE = dyn_cast<ConstantExpr>(unwrap(Inst)))
		if (CE->getOpcode() == Instruction::FCmp)
			return (LLVMRealPredicate)CE->getPredicate();
	return (LLVMRealPredicate)0;
}

unsigned LLVM_Hs_CountInstStructureIndices(LLVMValueRef v) {
	if (ExtractValueInst *i = dyn_cast<ExtractValueInst>(unwrap(v))) return i->getNumIndices();
	if (InsertValueInst *i = dyn_cast<InsertValueInst>(unwrap(v))) return i->getNumIndices();
	return 0;
}

void LLVM_Hs_GetInstStructureIndices(LLVMValueRef v, unsigned *is) {
	ArrayRef<unsigned> a;
	if (ExtractValueInst *i = dyn_cast<ExtractValueInst>(unwrap(v))) a = i->getIndices();
	if (InsertValueInst *i = dyn_cast<InsertValueInst>(unwrap(v))) a = i->getIndices();
	std::copy(a.begin(), a.end(), is);
}

void LLVM_Hs_GetSwitchCases(
	LLVMValueRef v,
	LLVMValueRef *values,
	LLVMBasicBlockRef *dests
) {
	SwitchInst *s = unwrap<SwitchInst>(v);
	for(SwitchInst::CaseIt i = s->case_begin(); i != s->case_end(); ++i, ++values, ++dests) {
		*values = wrap(i->getCaseValue());
		*dests = wrap(i->getCaseSuccessor());
	}
}

void LLVM_Hs_GetIndirectBrDests(
	LLVMValueRef v,
	LLVMBasicBlockRef *dests
) {
	IndirectBrInst *ib = unwrap<IndirectBrInst>(v);
	for(unsigned i=0; i != ib->getNumDestinations(); ++i, ++dests)
		*dests = wrap(ib->getDestination(i));
}

void LLVM_Hs_SetMetadata(LLVMValueRef inst, unsigned kindID, LLVMMetadataRef md) {
    MDNode *N = md ? unwrap<MDNode>(md) : nullptr;
    unwrap<Instruction>(inst)->setMetadata(kindID, N);
}

unsigned LLVM_Hs_GetMetadata(
	LLVMValueRef i,
	unsigned *kinds,
	LLVMMetadataRef *nodes,
	unsigned nKinds
) {
	SmallVector<std::pair<unsigned, MDNode *>, 4> mds;
	unwrap<Instruction>(i)->getAllMetadata(mds);
	if (mds.size() <= nKinds) {
		for(unsigned i=0; i<mds.size(); ++i) {
			kinds[i] = mds[i].first;
			nodes[i] = wrap(mds[i].second);
		}
	}
	return mds.size();
}

LLVMValueRef LLVM_Hs_GetCleanupPad(LLVMValueRef i) {
    return wrap(unwrap<CleanupReturnInst>(i)->getCleanupPad());
}

LLVMBasicBlockRef LLVM_Hs_GetUnwindDest(LLVMValueRef i) {
    return wrap(unwrap<CleanupReturnInst>(i)->getUnwindDest());
}

LLVMValueRef LLVM_Hs_GetParentPad(LLVMValueRef i) {
    return wrap(unwrap<FuncletPadInst>(i)->getParentPad());
}

unsigned LLVM_Hs_GetNumArgOperands(LLVMValueRef i) {
    return unwrap<FuncletPadInst>(i)->getNumArgOperands();
}

LLVMValueRef LLVM_Hs_GetArgOperand(LLVMValueRef i, unsigned op) {
    return wrap(unwrap<FuncletPadInst>(i)->getArgOperand(op));
}

LLVMValueRef LLVM_Hs_CatchSwitch_GetParentPad(LLVMValueRef i) {
    return wrap(unwrap<CatchSwitchInst>(i)->getParentPad());
}

LLVMBasicBlockRef LLVM_Hs_CatchSwitch_GetUnwindDest(LLVMValueRef i) {
    return wrap(unwrap<CatchSwitchInst>(i)->getUnwindDest());
}

unsigned LLVM_Hs_CatchSwitch_GetNumHandlers(LLVMValueRef i) {
    return unwrap<CatchSwitchInst>(i)->getNumHandlers();
}

LLVMBasicBlockRef LLVM_Hs_CatchSwitch_GetHandler(LLVMValueRef instr, unsigned i) {
    return wrap(*(unwrap<CatchSwitchInst>(instr)->handler_begin() + i));
}

void LLVM_Hs_CatchSwitch_AddHandler(LLVMValueRef instr, LLVMBasicBlockRef bb) {
    unwrap<CatchSwitchInst>(instr)->addHandler(unwrap(bb));
}

LLVMValueRef LLVM_Hs_CatchRet_GetCatchPad(LLVMValueRef i) {
    return wrap(unwrap<CatchReturnInst>(i)->getCatchPad());
}

LLVMBasicBlockRef LLVM_Hs_CatchRet_GetSuccessor(LLVMValueRef i) {
    return wrap(unwrap<CatchReturnInst>(i)->getSuccessor());
}
}
