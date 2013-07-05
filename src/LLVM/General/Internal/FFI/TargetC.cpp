#define __STDC_LIMIT_MACROS
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/Host.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/ADT/Triple.h"
#include "llvm/ExecutionEngine/Interpreter.h"
#include "llvm/IR/DataLayout.h"
#include "llvm-c/Target.h"
#include "llvm-c/TargetMachine.h"
#include "llvm-c/Core.h"
#include "LLVM/General/Internal/FFI/Target.h"

using namespace llvm;

namespace llvm {
// Taken from llvm/lib/Target/TargetMachineC.cpp
static Target *unwrap(LLVMTargetRef P) {
  return reinterpret_cast<Target*>(P);
}
// Taken from llvm/lib/Target/TargetMachineC.cpp
static LLVMTargetRef wrap(const Target * P) {
  return reinterpret_cast<LLVMTargetRef>(const_cast<Target*>(P));
}
// Taken from llvm/lib/Target/TargetMachineC.cpp
inline TargetMachine *unwrap(LLVMTargetMachineRef P) {
  return reinterpret_cast<TargetMachine*>(P);
}
// Taken from llvm/lib/Target/TargetMachineC.cpp
inline LLVMTargetMachineRef wrap(const TargetMachine *P) {
  return
    reinterpret_cast<LLVMTargetMachineRef>(const_cast<TargetMachine*>(P));
}

static Reloc::Model unwrap(LLVMRelocMode x) {
	switch(x) {
#define ENUM_CASE(x,y) case LLVMReloc ## x: return Reloc::y;
LLVM_GENERAL_FOR_EACH_RELOC_MODEL(ENUM_CASE)
#undef ENUM_CASE
	default: return Reloc::Model(0);
	}
}

static CodeGenOpt::Level unwrap(LLVMCodeGenOptLevel x) {
	switch(x) {
#define ENUM_CASE(x) case LLVMCodeGenLevel ## x: return CodeGenOpt::x;
LLVM_GENERAL_FOR_EACH_CODE_GEN_OPT_LEVEL(ENUM_CASE)
#undef ENUM_CASE
	default: return CodeGenOpt::Level(0);
	}
}

static FloatABI::ABIType unwrap(LLVM_General_FloatABI x) {
	switch(x) {
#define ENUM_CASE(x) case LLVM_General_FloatABI_ ## x: return FloatABI::x;
LLVM_GENERAL_FOR_EACH_FLOAT_ABI(ENUM_CASE)
#undef ENUM_CASE
	default: return FloatABI::ABIType(0);
	}
}

static LLVM_General_FloatABI wrap(FloatABI::ABIType x) {
	switch(x) {
#define ENUM_CASE(x) case FloatABI::x: return LLVM_General_FloatABI_ ## x;
LLVM_GENERAL_FOR_EACH_FLOAT_ABI(ENUM_CASE)
#undef ENUM_CASE
	default: return LLVM_General_FloatABI(0);
	}
}

static FPOpFusion::FPOpFusionMode unwrap(LLVM_General_FPOpFusionMode x) {
	switch(x) {
#define ENUM_CASE(x) case LLVM_General_FPOpFusionMode_ ## x: return FPOpFusion::x;
LLVM_GENERAL_FOR_EACH_FP_OP_FUSION_MODE(ENUM_CASE)
#undef ENUM_CASE
	default: return FPOpFusion::FPOpFusionMode(0);
	}
}

static LLVM_General_FPOpFusionMode wrap(FPOpFusion::FPOpFusionMode x) {
	switch(x) {
#define ENUM_CASE(x) case FPOpFusion::x: return LLVM_General_FPOpFusionMode_ ## x;
LLVM_GENERAL_FOR_EACH_FP_OP_FUSION_MODE(ENUM_CASE)
#undef ENUM_CASE
	default: return  LLVM_General_FPOpFusionMode(0);
	}
}
}

extern "C" {

LLVMBool LLVM_General_InitializeNativeTarget() {
	return LLVMInitializeNativeTarget()
    || InitializeNativeTargetAsmPrinter()
    || InitializeNativeTargetAsmParser();
}

LLVMTargetRef LLVM_General_LookupTarget(
	const char *arch, 
	const char *ctriple, 
	const char **tripleOut, 
	const char **cerror
) {
	std::string error;
	Triple triple(ctriple);
	if (const Target *result = TargetRegistry::lookupTarget(arch, triple, error)) {
		*tripleOut = strdup(triple.getTriple().c_str());
		return wrap(result);
	}
	*cerror = strdup(error.c_str());
	return 0;
}

TargetOptions *LLVM_General_CreateTargetOptions() {
	TargetOptions *to = new TargetOptions();
	to->SSPBufferSize = 0; // this field was left uninitialized in LLVM 3.2
	return to;
}

void LLVM_General_SetTargetOptionFlag(
	TargetOptions *to,
	LLVM_General_TargetOptionFlag f,
	unsigned v
) {
	switch(f) {
#define ENUM_CASE(op) case LLVM_General_TargetOptionFlag_ ## op: to->op = v ? 1 : 0; break;
	LLVM_GENERAL_FOR_EACH_TARGET_OPTION_FLAG(ENUM_CASE)
#undef ENUM_CASE
	}
}

unsigned LLVM_General_GetTargetOptionFlag(
	TargetOptions *to,
	LLVM_General_TargetOptionFlag f
) {
	switch(f) {
#define ENUM_CASE(op) case LLVM_General_TargetOptionFlag_ ## op: return to->op;
	LLVM_GENERAL_FOR_EACH_TARGET_OPTION_FLAG(ENUM_CASE)
#undef ENUM_CASE
	}
}

void LLVM_General_SetStackAlignmentOverride(TargetOptions *to, unsigned v) {
	to->StackAlignmentOverride = v;
}

unsigned LLVM_General_GetStackAlignmentOverride(TargetOptions *to) {
	return to->StackAlignmentOverride;
}

void LLVM_General_SetTrapFuncName(TargetOptions *to, const char *v) {
	to->TrapFuncName = v;
}

const char *LLVM_General_GetTrapFuncName(TargetOptions *to) {
	return to->TrapFuncName.c_str();
}

void LLVM_General_SetFloatABIType(TargetOptions *to, LLVM_General_FloatABI v) {
	to->FloatABIType = unwrap(v);
}

LLVM_General_FloatABI LLVM_General_GetFloatABIType(TargetOptions *to) {
	return wrap(to->FloatABIType);
}

void LLVM_General_SetAllowFPOpFusion(TargetOptions *to, LLVM_General_FPOpFusionMode v) {
	to->AllowFPOpFusion = unwrap(v);
}

LLVM_General_FPOpFusionMode LLVM_General_GetAllowFPOpFusion(TargetOptions *to) {
	return wrap(to->AllowFPOpFusion);
}

void LLVM_General_SetSSPBufferSize(TargetOptions *to, unsigned v) {
	to->SSPBufferSize = v;
}

unsigned LLVM_General_GetSSPBufferSize(TargetOptions *to) {
	return to->SSPBufferSize;
}

void LLVM_General_DisposeTargetOptions(TargetOptions *t) {
	delete t;
}


LLVMTargetMachineRef LLVM_General_CreateTargetMachine(
	LLVMTargetRef target,
	const char *triple,
	const char *cpu,
	const char *features,
	const TargetOptions *targetOptions,
	LLVMRelocMode relocModel,
	LLVMCodeModel codeModel,
	LLVMCodeGenOptLevel codeGenOptLevel
) {
	return wrap(
		unwrap(target)->createTargetMachine(
			triple,
			cpu,
			features,
			*targetOptions, 
			unwrap(relocModel),
			unwrap(codeModel),
			unwrap(codeGenOptLevel)
		)
	);
}
	
const TargetLowering *LLVM_General_GetTargetLowering(LLVMTargetMachineRef t) {
	return unwrap(t)->getTargetLowering();
}

char *LLVM_General_GetDefaultTargetTriple() {
	return strdup(sys::getDefaultTargetTriple().c_str());
}

char *LLVM_General_GetProcessTargetTriple() {
	return strdup(sys::getProcessTriple().c_str());
}

char *LLVM_General_GetHostCPUName() {
	return strdup(sys::getHostCPUName().c_str());
}

char *LLVM_General_GetHostCPUFeatures() {
	StringMap<bool> featureMap;
	std::string features;
	if (sys::getHostCPUFeatures(featureMap)) {
		for(llvm::StringMap<bool>::const_iterator it = featureMap.begin(); it != featureMap.end(); ++it) {
			if (it->second) {
				features += it->first().str() + " ";
			}
		}
	}
	return strdup(features.c_str());
}

char *LLVM_General_GetTargetMachineDataLayout(LLVMTargetMachineRef t) {
	return strdup(unwrap(t)->getDataLayout()->getStringRepresentation().c_str());
}

void LLVM_General_InitializeAllTargets() {
	InitializeAllTargetInfos();
	InitializeAllTargets();
	InitializeAllTargetMCs();
	InitializeAllAsmPrinters();
	// None of the other components are bound yet
}

}
