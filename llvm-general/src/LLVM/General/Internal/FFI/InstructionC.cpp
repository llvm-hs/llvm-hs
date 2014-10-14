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

#include "LLVM/General/Internal/FFI/Instruction.h"

using namespace llvm;

namespace llvm {

static LLVMAtomicOrdering wrap(AtomicOrdering l) {
	switch(l) {
#define ENUM_CASE(x) case x: return LLVMAtomicOrdering ## x;
LLVM_GENERAL_FOR_EACH_ATOMIC_ORDERING(ENUM_CASE)
#undef ENUM_CASE
	default: return LLVMAtomicOrdering(0);
	}
}

static LLVMSynchronizationScope wrap(SynchronizationScope l) {
	switch(l) {
#define ENUM_CASE(x) case x: return LLVM ## x ## SynchronizationScope;
LLVM_GENERAL_FOR_EACH_SYNCRONIZATION_SCOPE(ENUM_CASE)
#undef ENUM_CASE
	default: return LLVMSynchronizationScope(0);
	}
}

static LLVMAtomicRMWBinOp wrap(AtomicRMWInst::BinOp l) {
	switch(l) {
#define ENUM_CASE(x) case AtomicRMWInst::x: return LLVMAtomicRMWBinOp ## x;
LLVM_GENERAL_FOR_EACH_RMW_OPERATION(ENUM_CASE)
#undef ENUM_CASE
	default: return LLVMAtomicRMWBinOp(0);
	}
}

LLVMFastMathFlags wrap(FastMathFlags f) {
	unsigned r = 0;
#define ENUM_CASE(u,l) if (f.l()) r |= unsigned(LLVM ## u);
LLVM_GENERAL_FOR_EACH_FAST_MATH_FLAG(ENUM_CASE)
#undef ENUM_CASE
	return LLVMFastMathFlags(r);
}

}

extern "C" {

unsigned LLVM_General_GetInstructionDefOpcode(LLVMValueRef val) {
	return unwrap<Instruction>(val)->getOpcode();
}

unsigned LLVM_General_HasNoSignedWrap(LLVMValueRef val) {
	return unwrap<OverflowingBinaryOperator>(val)->hasNoSignedWrap();
}

unsigned LLVM_General_HasNoUnsignedWrap(LLVMValueRef val) {
	return unwrap<OverflowingBinaryOperator>(val)->hasNoUnsignedWrap();
}

int LLVM_General_IsExact(LLVMValueRef val) {
	return unwrap<PossiblyExactOperator>(val)->isExact();
}

LLVMFastMathFlags LLVM_General_GetFastMathFlags(LLVMValueRef val) {
	return wrap(unwrap<Instruction>(val)->getFastMathFlags());
}

LLVMValueRef LLVM_General_GetCallInstCalledValue(
	LLVMValueRef callInst
) {
	return wrap(CallSite(unwrap<Instruction>(callInst)).getCalledValue());
}

LLVMAttribute LLVM_General_GetCallInstAttr(LLVMValueRef callInst, unsigned i) {
	return (LLVMAttribute)CallSite(unwrap<Instruction>(callInst)).getAttributes().Raw(i);
}

void LLVM_General_AddCallInstAttr(LLVMValueRef callInst, unsigned i, LLVMAttribute attr) {
	CallSite callSite(unwrap<Instruction>(callInst));
	LLVMContext &context = callSite->getContext();
	AttrBuilder attrBuilder(attr);
	callSite.setAttributes(callSite.getAttributes().addAttributes(context, i, AttributeSet::get(context, i, attrBuilder)));
}

LLVMAttribute LLVM_General_GetCallInstFunctionAttr(LLVMValueRef callInst) {
	return LLVM_General_GetCallInstAttr(callInst, AttributeSet::FunctionIndex);
}

void LLVM_General_AddCallInstFunctionAttr(LLVMValueRef callInst, LLVMAttribute attr) {
	LLVM_General_AddCallInstAttr(callInst, AttributeSet::FunctionIndex, attr);
}

LLVMValueRef LLVM_General_GetAllocaNumElements(LLVMValueRef a) {
	return wrap(unwrap<AllocaInst>(a)->getArraySize());
}

LLVMTypeRef LLVM_General_GetAllocatedType(LLVMValueRef a) {
	return wrap(unwrap<AllocaInst>(a)->getAllocatedType());
}

// ------------------------------------------------------------

#define LLVM_GENERAL_FOR_EACH_ALIGNMENT_INST(macro) \
	macro(Alloca) \
	macro(Load) \
	macro(Store)

unsigned LLVM_General_GetInstrAlignment(LLVMValueRef l) {
	switch(unwrap<Instruction>(l)->getOpcode()) {
#define ENUM_CASE(n) case Instruction::n: return unwrap<n ## Inst>(l)->getAlignment();
		LLVM_GENERAL_FOR_EACH_ALIGNMENT_INST(ENUM_CASE)
#undef ENUM_CASE
	default: return 0;
	}
}

void LLVM_General_SetInstrAlignment(LLVMValueRef l, unsigned a) {
	switch(unwrap<Instruction>(l)->getOpcode()) {
#define ENUM_CASE(n) case Instruction::n: unwrap<n ## Inst>(l)->setAlignment(a); break;
		LLVM_GENERAL_FOR_EACH_ALIGNMENT_INST(ENUM_CASE)
#undef ENUM_CASE
	}
}

// ------------------------------------------------------------

#define LLVM_GENERAL_FOR_EACH_ATOMIC_INST(macro)	\
	macro(Load,)																		\
	macro(Store,)																		\
	macro(Fence,)																		\
	macro(AtomicCmpXchg,Success)										\
	macro(AtomicRMW,)

LLVMAtomicOrdering LLVM_General_GetAtomicOrdering(LLVMValueRef i) {
	switch(unwrap<Instruction>(i)->getOpcode()) {
#define ENUM_CASE(n,s) case Instruction::n: return wrap(unwrap<n ## Inst>(i)->get ## s ## Ordering());
		LLVM_GENERAL_FOR_EACH_ATOMIC_INST(ENUM_CASE)
#undef ENUM_CASE
	default: return LLVMAtomicOrdering(0);
	}
}

LLVMAtomicOrdering LLVM_General_GetFailureAtomicOrdering(LLVMValueRef i) {
	switch(unwrap<Instruction>(i)->getOpcode()) {
	case Instruction::AtomicCmpXchg: return wrap(unwrap<AtomicCmpXchgInst>(i)->getFailureOrdering());
	default: return LLVMAtomicOrdering(0);
	}
}

LLVMSynchronizationScope LLVM_General_GetSynchronizationScope(LLVMValueRef i) {
	switch(unwrap<Instruction>(i)->getOpcode()) {
#define ENUM_CASE(n,s) case Instruction::n: return wrap(unwrap<n ## Inst>(i)->getSynchScope());
		LLVM_GENERAL_FOR_EACH_ATOMIC_INST(ENUM_CASE)
#undef ENUM_CASE
	default: return LLVMSynchronizationScope(0);
	}
}

#define LLVM_GENERAL_FOR_EACH_VOLATILITY_INST(macro) \
	macro(Load) \
	macro(Store) \
	macro(AtomicCmpXchg) \
	macro(AtomicRMW)

LLVMBool LLVM_General_GetVolatile(LLVMValueRef i) {
	switch(unwrap<Instruction>(i)->getOpcode()) {
#define ENUM_CASE(n) case Instruction::n: return unwrap<n ## Inst>(i)->isVolatile();
		LLVM_GENERAL_FOR_EACH_VOLATILITY_INST(ENUM_CASE)
#undef ENUM_CASE
	default: return LLVMBool(0);
	}
}

// ------------------------------------------------------------

LLVMBool LLVM_General_GetInBounds(LLVMValueRef i) {
	return unwrap<GEPOperator>(i)->isInBounds();
}

LLVMAtomicRMWBinOp LLVM_General_GetAtomicRMWBinOp(LLVMValueRef i) {
	return wrap(unwrap<AtomicRMWInst>(i)->getOperation());
}

LLVMRealPredicate LLVM_General_GetFCmpPredicate(LLVMValueRef Inst) {
	if (FCmpInst *I = dyn_cast<FCmpInst>(unwrap(Inst)))
		return (LLVMRealPredicate)I->getPredicate();
	if (ConstantExpr *CE = dyn_cast<ConstantExpr>(unwrap(Inst)))
		if (CE->getOpcode() == Instruction::FCmp)
			return (LLVMRealPredicate)CE->getPredicate();
	return (LLVMRealPredicate)0;
}

unsigned LLVM_General_CountInstStructureIndices(LLVMValueRef v) {
	if (ExtractValueInst *i = dyn_cast<ExtractValueInst>(unwrap(v))) return i->getNumIndices();
	if (InsertValueInst *i = dyn_cast<InsertValueInst>(unwrap(v))) return i->getNumIndices();
	return 0;
}

void LLVM_General_GetInstStructureIndices(LLVMValueRef v, unsigned *is) {
	ArrayRef<unsigned> a;
	if (ExtractValueInst *i = dyn_cast<ExtractValueInst>(unwrap(v))) a = i->getIndices();
	if (InsertValueInst *i = dyn_cast<InsertValueInst>(unwrap(v))) a = i->getIndices();
	std::copy(a.begin(), a.end(), is);
}

LLVMBool LLVM_General_IsCleanup(LLVMValueRef v) {
	return unwrap<LandingPadInst>(v)->isCleanup();
}

void LLVM_General_GetSwitchCases(
	LLVMValueRef v,
	LLVMValueRef *values,
	LLVMBasicBlockRef *dests
) {
	SwitchInst *s = unwrap<SwitchInst>(v);
	for(SwitchInst::CaseIt i = s->case_begin(); i != s->case_end(); ++i, ++values, ++dests) {
		*values = wrap(i.getCaseValue());
		*dests = wrap(i.getCaseSuccessor());
	}
}

void LLVM_General_GetIndirectBrDests(
	LLVMValueRef v,
	LLVMBasicBlockRef *dests
) {
	IndirectBrInst *ib = unwrap<IndirectBrInst>(v);
	for(unsigned i=0; i != ib->getNumDestinations(); ++i, ++dests)
		*dests = wrap(ib->getDestination(i));
}

unsigned LLVM_General_GetMetadata(
	LLVMValueRef i,
	unsigned *kinds,
	LLVMValueRef *nodes,
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

}

