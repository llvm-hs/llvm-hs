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
#include "llvm/Transforms/Instrumentation/AddressSanitizer.h"
#include "llvm/Transforms/Instrumentation/BoundsChecking.h"
#include "llvm/Transforms/Instrumentation/MemorySanitizer.h"
#include "llvm/Transforms/Instrumentation/ThreadSanitizer.h"
#include "llvm/Transforms/Utils.h"
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

#define LLVM_HS_FOR_EACH_PASS_WITHOUT_LLVM_C_BINDING(macro) \
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
void LLVM_Hs_Add ## p ## Pass(LLVMPassManagerRef PM) {	\
	unwrap(PM)->add(create ## p ## Pass());			\
}
LLVM_HS_FOR_EACH_PASS_WITHOUT_LLVM_C_BINDING(ENUM_CASE)
#undef ENUM_CASE

void LLVM_Hs_AddCodeGenPreparePass(LLVMPassManagerRef PM) {
	unwrap(PM)->add(createCodeGenPreparePass());
}

void LLVM_Hs_AddGlobalValueNumberingPass(LLVMPassManagerRef PM, LLVMBool noLoads) {
	unwrap(PM)->add(createGVNPass(noLoads));
}

void LLVM_Hs_AddInternalizePass(LLVMPassManagerRef PM, unsigned nExports, const char **exports) {
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

void LLVM_Hs_AddLoopStrengthReducePass(LLVMPassManagerRef PM) {
	unwrap(PM)->add(createLoopStrengthReducePass());
}

void LLVM_Hs_AddLowerInvokePass(LLVMPassManagerRef PM) {
	unwrap(PM)->add(createLowerInvokePass());
}

void LLVM_Hs_AddSROAPass(LLVMPassManagerRef PM) {
	unwrap(PM)->add(createSROAPass());
}

void LLVM_Hs_AddGCOVProfilerPass(
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

void LLVM_Hs_AddAddressSanitizerFunctionPass(
	LLVMPassManagerRef PM
) {
	unwrap(PM)->add(createAddressSanitizerFunctionPass());
}

void LLVM_Hs_AddAddressSanitizerModulePass(
	LLVMPassManagerRef PM
) {
	unwrap(PM)->add(createModuleAddressSanitizerLegacyPassPass());
}

void LLVM_Hs_AddMemorySanitizerPass(
	LLVMPassManagerRef PM,
	LLVMBool trackOrigins,
    LLVMBool recover,
    LLVMBool kernel
) {
	unwrap(PM)->add(createMemorySanitizerLegacyPassPass({trackOrigins, static_cast<bool>(recover), static_cast<bool>(kernel)}));
}

void LLVM_Hs_AddThreadSanitizerPass(
	LLVMPassManagerRef PM
) {
	unwrap(PM)->add(createThreadSanitizerLegacyPassPass());
}

void LLVM_Hs_AddBoundsCheckingPass(LLVMPassManagerRef PM) {
	unwrap(PM)->add(createBoundsCheckingLegacyPass());
}

void LLVM_Hs_AddLoopVectorizePass(
	LLVMPassManagerRef PM,
	LLVMBool interleaveOnlyWhenForced,
	LLVMBool vectorizeOnlyWhenForced
) {
	unwrap(PM)->add(createLoopVectorizePass(interleaveOnlyWhenForced, vectorizeOnlyWhenForced));
}

void LLVM_Hs_PassManagerBuilderSetLibraryInfo(
	LLVMPassManagerBuilderRef PMB,
	LLVMTargetLibraryInfoRef l
) {
	// The PassManager frees the TargetLibraryInfo when done,
	// but we also free our ref, so give it a new copy.
	unwrap(PMB)->LibraryInfo = new TargetLibraryInfoImpl(*unwrap(l));
}

void LLVM_Hs_PassManagerBuilderSetLoopVectorize(
	LLVMPassManagerBuilderRef PMB,
	LLVMBool runLoopVectorization
) {
	unwrap(PMB)->LoopVectorize = runLoopVectorization;
}

void LLVM_Hs_PassManagerBuilderSetSuperwordLevelParallelismVectorize(
	LLVMPassManagerBuilderRef PMB,
	LLVMBool runSLPVectorization
) {
	unwrap(PMB)->SLPVectorize = runSLPVectorization;
}

}
