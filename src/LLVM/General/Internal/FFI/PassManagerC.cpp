#define __STDC_LIMIT_MACROS
#include "llvm/IR/LLVMContext.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/Vectorize.h"
#include "llvm/PassManager.h"

#include "llvm-c/Core.h"

using namespace llvm;

extern "C" {
typedef struct LLVMOpaqueVectorizationConfig *LLVMVectorizationConfigRef;
typedef struct LLVMOpaqueTargetLowering *LLVMTargetLoweringRef;
typedef struct LLVMOpaqueTargetMachine *LLVMTargetMachineRef;
}

namespace llvm {
inline TargetLowering *unwrap(LLVMTargetLoweringRef P) { 
	return reinterpret_cast<TargetLowering *>(P);
}

inline LLVMTargetLoweringRef wrap(const TargetLowering *P) { 
	return reinterpret_cast<LLVMTargetLoweringRef>(const_cast<TargetLowering *>(P));
}

inline TargetMachine *unwrap(LLVMTargetMachineRef P) {
	return reinterpret_cast<TargetMachine*>(P);
}

inline LLVMTargetMachineRef wrap(const TargetMachine *P) {
	return reinterpret_cast<LLVMTargetMachineRef>(const_cast<TargetMachine *>(P));
}
}

extern "C" {

#define LLVM_GENERAL_FOR_EACH_PASS_WITHOUT_LLVM_C_BINDING(macro) \
	macro(BlockPlacement)			\
	macro(BreakCriticalEdges) \
	macro(DeadCodeElimination) \
	macro(DeadInstElimination) \
	macro(DemoteRegisterToMemory) \
	macro(LCSSA) \
	macro(LoopInstSimplify) \
	macro(LowerAtomic) \
	macro(LowerSwitch) \
	macro(MergeFunctions) \
	macro(PartialInlining) \
	macro(Sinking) \
	macro(StripDeadDebugInfo) \
	macro(StripDebugDeclare) \
	macro(StripNonDebugSymbols) \

#define ENUM_CASE(p)								\
void LLVM_General_Add ## p ## Pass(LLVMPassManagerRef PM) {	\
	unwrap(PM)->add(create ## p ## Pass());			\
}
LLVM_GENERAL_FOR_EACH_PASS_WITHOUT_LLVM_C_BINDING(ENUM_CASE)
#undef ENUM_CASE

void LLVM_General_AddCodeGenPreparePass(LLVMPassManagerRef PM, LLVMTargetMachineRef T) {
	unwrap(PM)->add(createCodeGenPreparePass(unwrap(T)));
}
	
void LLVM_General_AddGlobalValueNumberingPass(LLVMPassManagerRef PM, LLVMBool noLoads) {
	unwrap(PM)->add(createGVNPass(noLoads));
}

void LLVM_General_AddInternalizePass(LLVMPassManagerRef PM, unsigned nExports, const char **exports) {
	std::vector<const char *> exportList(exports, exports + nExports);
	unwrap(PM)->add(createInternalizePass(exportList));
}

void LLVM_General_AddLoopStrengthReducePass(LLVMPassManagerRef PM) {
	unwrap(PM)->add(createLoopStrengthReducePass());
}

void LLVM_General_AddLowerInvokePass(LLVMPassManagerRef PM, LLVMTargetMachineRef T, LLVMBool expensiveEH) {
	unwrap(PM)->add(createLowerInvokePass(unwrap(T), expensiveEH));
}
	
void LLVM_General_AddSROAPass(LLVMPassManagerRef PM, LLVMBool RequiresDomTree) {
	unwrap(PM)->add(createSROAPass(RequiresDomTree));
}

void LLVM_General_AddBasicBlockVectorizePass(
	LLVMPassManagerRef PM,
	unsigned vectorBits,
	LLVMBool vectorizeBools,
	LLVMBool vectorizeInts,
	LLVMBool vectorizeFloats,
	LLVMBool vectorizePointers,
	LLVMBool vectorizeCasts,
	LLVMBool vectorizeMath,
	LLVMBool vectorizeFusedMultiplyAdd,
	LLVMBool vectorizeSelect,
	LLVMBool vectorizeCmp,
	LLVMBool vectorizeGetElementPtr,
	LLVMBool vectorizeMemoryOperations,
	LLVMBool alignedOnly,
	unsigned reqChainDepth,
	unsigned searchLimit,
	unsigned maxCandidatePairsForCycleCheck,
	LLVMBool splatBreaksChain,
	unsigned maxInstructions,
	unsigned maxIterations,
	LLVMBool powerOfTwoLengthsOnly,
	LLVMBool noMemoryOperationBoost,
	LLVMBool fastDependencyAnalysis
) {
	VectorizeConfig vectorizeConfig;
	vectorizeConfig.VectorBits = vectorBits;
	vectorizeConfig.VectorizeBools = vectorizeBools;
	vectorizeConfig.VectorizeInts = vectorizeInts;
	vectorizeConfig.VectorizeFloats = vectorizeFloats;
	vectorizeConfig.VectorizePointers = vectorizePointers;
	vectorizeConfig.VectorizeCasts = vectorizeCasts;
	vectorizeConfig.VectorizeMath = vectorizeMath;
	vectorizeConfig.VectorizeFMA = vectorizeFusedMultiplyAdd;
	vectorizeConfig.VectorizeSelect = vectorizeSelect;
	vectorizeConfig.VectorizeCmp = vectorizeCmp;
	vectorizeConfig.VectorizeGEP = vectorizeGetElementPtr;
	vectorizeConfig.VectorizeMemOps = vectorizeMemoryOperations;
	vectorizeConfig.AlignedOnly = alignedOnly;
	vectorizeConfig.ReqChainDepth = reqChainDepth;
	vectorizeConfig.SearchLimit = searchLimit;
	vectorizeConfig.MaxCandPairsForCycleCheck = maxCandidatePairsForCycleCheck;
	vectorizeConfig.SplatBreaksChain = splatBreaksChain;
	vectorizeConfig.MaxInsts = maxInstructions;
	vectorizeConfig.MaxIter = maxIterations;
	vectorizeConfig.Pow2LenOnly = powerOfTwoLengthsOnly;
	vectorizeConfig.NoMemOpBoost = noMemoryOperationBoost;
	vectorizeConfig.FastDep = fastDependencyAnalysis;	

	unwrap(PM)->add(createBBVectorizePass(vectorizeConfig));
}	

}
