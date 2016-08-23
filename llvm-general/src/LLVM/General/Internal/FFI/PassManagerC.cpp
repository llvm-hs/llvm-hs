#define __STDC_LIMIT_MACROS
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/IPO/Internalize.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/Vectorize.h"
#include "llvm/Transforms/Instrumentation.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm-c/Target.h"
#include "llvm-c/Transforms/PassManagerBuilder.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Target/TargetMachine.h"

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

// Taken from llvm/lib/Transforms/IPO/PassManagerBuilder.cpp
inline PassManagerBuilder *unwrap(LLVMPassManagerBuilderRef P) {
    return reinterpret_cast<PassManagerBuilder*>(P);
}

inline TargetLibraryInfoImpl *unwrap(LLVMTargetLibraryInfoRef P) {
  return reinterpret_cast<TargetLibraryInfoImpl*>(P);
}

}

extern "C" {

#define LLVM_GENERAL_FOR_EACH_PASS_WITHOUT_LLVM_C_BINDING(macro) \
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
    std::vector<std::string> exportList(nExports);
    for (unsigned i = 0; i < nExports; ++i) {
        exportList.at(i) = exports[i];
    }
    std::function<bool(const GlobalValue &)> mustPreserveGV = [exportList](const GlobalValue & gv) {
        for (const auto& exp : exportList) {
            if (gv.getName().equals(exp)) {
                return true;
            }
        }
        return false;
    };
    unwrap(PM)->add(createInternalizePass(std::move(mustPreserveGV)));
}

void LLVM_General_AddLoopStrengthReducePass(LLVMPassManagerRef PM) {
	unwrap(PM)->add(createLoopStrengthReducePass());
}

void LLVM_General_AddLowerInvokePass(LLVMPassManagerRef PM) {
	unwrap(PM)->add(createLowerInvokePass());
}
	
void LLVM_General_AddSROAPass(LLVMPassManagerRef PM) {
	unwrap(PM)->add(createSROAPass());
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

void LLVM_General_AddGCOVProfilerPass(
	LLVMPassManagerRef PM,
	LLVMBool emitNotes,
	LLVMBool emitData,
	const char *version,
	LLVMBool useCfgChecksum,
	LLVMBool noRedZone,
	LLVMBool functionNamesInData
) {
	struct GCOVOptions options;
	options.EmitNotes = emitNotes;
	options.EmitData = emitData;
	std::copy(version, version+4, options.Version);
	options.UseCfgChecksum = useCfgChecksum;
	options.NoRedZone = noRedZone;
	options.FunctionNamesInData = functionNamesInData;
	unwrap(PM)->add(createGCOVProfilerPass(options));
}

void LLVM_General_AddAddressSanitizerFunctionPass(
	LLVMPassManagerRef PM
) {
	unwrap(PM)->add(createAddressSanitizerFunctionPass());
}

void LLVM_General_AddAddressSanitizerModulePass(
	LLVMPassManagerRef PM
) {
	unwrap(PM)->add(createAddressSanitizerModulePass());
}

void LLVM_General_AddMemorySanitizerPass(
	LLVMPassManagerRef PM,
	LLVMBool trackOrigins
) {
	unwrap(PM)->add(createMemorySanitizerPass(trackOrigins));
}

void LLVM_General_AddThreadSanitizerPass(
	LLVMPassManagerRef PM
) {
	unwrap(PM)->add(createThreadSanitizerPass());
}

void LLVM_General_AddBoundsCheckingPass(LLVMPassManagerRef PM) {
	unwrap(PM)->add(createBoundsCheckingPass());
}

void LLVM_General_AddLoopVectorizePass(
	LLVMPassManagerRef PM,
	LLVMBool noUnrolling,
	LLVMBool alwaysVectorize
) {
	unwrap(PM)->add(createLoopVectorizePass(noUnrolling, alwaysVectorize));
}

void LLVM_General_PassManagerBuilderSetLibraryInfo(
	LLVMPassManagerBuilderRef PMB,
	LLVMTargetLibraryInfoRef l
) {
	// The PassManager frees the TargetLibraryInfo when done,
	// but we also free our ref, so give it a new copy.
	unwrap(PMB)->LibraryInfo = new TargetLibraryInfoImpl(*unwrap(l));
}

void LLVM_General_PassManagerBuilderSetLoopVectorize(
	LLVMPassManagerBuilderRef PMB,
	LLVMBool runLoopVectorization
) {
	unwrap(PMB)->LoopVectorize = runLoopVectorization;
}

void LLVM_General_PassManagerBuilderSetSuperwordLevelParallelismVectorize(
	LLVMPassManagerBuilderRef PMB,
	LLVMBool runSLPVectorization
) {
	unwrap(PMB)->SLPVectorize = runSLPVectorization;
}

}
