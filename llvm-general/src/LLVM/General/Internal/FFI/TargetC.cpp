#define __STDC_LIMIT_MACROS
#include "llvm-c/Target.h"
#include "LLVM/General/Internal/FFI/LibFunc.h"
#include "LLVM/General/Internal/FFI/Target.h"
#include "LLVM/General/Internal/FFI/Target.hpp"
#include "llvm-c/Core.h"
#include "llvm-c/TargetMachine.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/ExecutionEngine/Interpreter.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"

using namespace llvm;

namespace llvm {
// Taken from llvm/lib/Target/TargetMachineC.cpp
static Target *unwrap(LLVMTargetRef P) { return reinterpret_cast<Target *>(P); }
// Taken from llvm/lib/Target/TargetMachineC.cpp
static LLVMTargetRef wrap(const Target *P) {
  return reinterpret_cast<LLVMTargetRef>(const_cast<Target *>(P));
}

inline TargetLibraryInfoImpl *unwrap(LLVMTargetLibraryInfoRef P) {
  return reinterpret_cast<TargetLibraryInfoImpl*>(P);
}

static CodeGenOpt::Level unwrap(LLVMCodeGenOptLevel x) {
  switch (x) {
#define ENUM_CASE(x)                                                           \
  case LLVMCodeGenLevel##x:                                                    \
    return CodeGenOpt::x;
    LLVM_GENERAL_FOR_EACH_CODE_GEN_OPT_LEVEL(ENUM_CASE)
#undef ENUM_CASE
  default:
    return CodeGenOpt::Level(0);
  }
}

static FloatABI::ABIType unwrap(LLVM_General_FloatABI x) {
  switch (x) {
#define ENUM_CASE(x)                                                           \
  case LLVM_General_FloatABI_##x:                                              \
    return FloatABI::x;
    LLVM_GENERAL_FOR_EACH_FLOAT_ABI(ENUM_CASE)
#undef ENUM_CASE
  default:
    return FloatABI::ABIType(0);
  }
}

static LibFunc::Func unwrap(LLVMLibFunc x) {
  switch (x) {
#define ENUM_CASE(x)                                                           \
  case LLVMLibFunc__##x:                                                       \
    return LibFunc::x;
    LLVM_GENERAL_FOR_EACH_LIB_FUNC(ENUM_CASE)
#undef ENUM_CASE
  default:
    return LibFunc::Func(0);
  }
}

static LLVMLibFunc wrap(LibFunc::Func x) {
  switch (x) {
#define ENUM_CASE(x)                                                           \
  case LibFunc::x:                                                             \
    return LLVMLibFunc__##x;
    LLVM_GENERAL_FOR_EACH_LIB_FUNC(ENUM_CASE)
#undef ENUM_CASE
  default:
    return LLVMLibFunc(0);
  }
}

static LLVM_General_FloatABI wrap(FloatABI::ABIType x) {
  switch (x) {
#define ENUM_CASE(x)                                                           \
  case FloatABI::x:                                                            \
    return LLVM_General_FloatABI_##x;
    LLVM_GENERAL_FOR_EACH_FLOAT_ABI(ENUM_CASE)
#undef ENUM_CASE
  default:
    return LLVM_General_FloatABI(0);
  }
}

static FPOpFusion::FPOpFusionMode unwrap(LLVM_General_FPOpFusionMode x) {
  switch (x) {
#define ENUM_CASE(x)                                                           \
  case LLVM_General_FPOpFusionMode_##x:                                        \
    return FPOpFusion::x;
    LLVM_GENERAL_FOR_EACH_FP_OP_FUSION_MODE(ENUM_CASE)
#undef ENUM_CASE
  default:
    return FPOpFusion::FPOpFusionMode(0);
  }
}

static LLVM_General_FPOpFusionMode wrap(FPOpFusion::FPOpFusionMode x) {
  switch (x) {
#define ENUM_CASE(x)                                                           \
  case FPOpFusion::x:                                                          \
    return LLVM_General_FPOpFusionMode_##x;
    LLVM_GENERAL_FOR_EACH_FP_OP_FUSION_MODE(ENUM_CASE)
#undef ENUM_CASE
  default:
    return LLVM_General_FPOpFusionMode(0);
  }
}

static TargetMachine::CodeGenFileType unwrap(LLVMCodeGenFileType x) {
  switch (x) {
#define ENUM_CASE(x)                                                           \
  case LLVM##x##File:                                                          \
    return TargetMachine::CGFT_##x##File;
    LLVM_GENERAL_FOR_EACH_CODE_GEN_FILE_TYPE(ENUM_CASE)
#undef ENUM_CASE
  default:
    return TargetMachine::CodeGenFileType(0);
  }
}
}

extern "C" {

LLVMBool LLVM_General_InitializeNativeTarget() {
  return LLVMInitializeNativeTarget() || InitializeNativeTargetAsmPrinter() ||
         InitializeNativeTargetAsmParser();
}

LLVMTargetRef LLVM_General_LookupTarget(const char *arch, const char *ctriple,
                                        const char **tripleOut,
                                        const char **cerror) {
  std::string error;
  Triple triple(ctriple);
  if (const Target *result =
          TargetRegistry::lookupTarget(arch, triple, error)) {
    *tripleOut = strdup(triple.getTriple().c_str());
    return wrap(result);
  }
  *cerror = strdup(error.c_str());
  return 0;
}

TargetOptions *LLVM_General_CreateTargetOptions() {
  TargetOptions *to = new TargetOptions();
  return to;
}

void LLVM_General_SetTargetOptionFlag(TargetOptions *to,
                                      LLVM_General_TargetOptionFlag f,
                                      unsigned v) {
  switch (f) {
#define ENUM_CASE(op)                                                          \
  case LLVM_General_TargetOptionFlag_##op:                                     \
    to->op = v ? 1 : 0;                                                        \
    break;
    LLVM_GENERAL_FOR_EACH_TARGET_OPTION_FLAG(ENUM_CASE)
#undef ENUM_CASE
  }
}

unsigned LLVM_General_GetTargetOptionFlag(TargetOptions *to,
                                          LLVM_General_TargetOptionFlag f) {
  switch (f) {
#define ENUM_CASE(op)                                                          \
  case LLVM_General_TargetOptionFlag_##op:                                     \
    return to->op;
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

// TODO (cocreature): See
// http://lists.llvm.org/pipermail/llvm-commits/Week-of-Mon-20150629/285229.html
// void LLVM_General_SetTrapFuncName(TargetOptions *to, const char *v) {
// 	to->TrapFuncName = v;
// }

// const char *LLVM_General_GetTrapFuncName(TargetOptions *to) {
// 	return to->TrapFuncName.c_str();
// }

void LLVM_General_SetFloatABIType(TargetOptions *to, LLVM_General_FloatABI v) {
  to->FloatABIType = unwrap(v);
}

LLVM_General_FloatABI LLVM_General_GetFloatABIType(TargetOptions *to) {
  return wrap(to->FloatABIType);
}

void LLVM_General_SetAllowFPOpFusion(TargetOptions *to,
                                     LLVM_General_FPOpFusionMode v) {
  to->AllowFPOpFusion = unwrap(v);
}

LLVM_General_FPOpFusionMode LLVM_General_GetAllowFPOpFusion(TargetOptions *to) {
  return wrap(to->AllowFPOpFusion);
}

void LLVM_General_DisposeTargetOptions(TargetOptions *t) { delete t; }

// const TargetLowering *LLVM_General_GetTargetLowering(LLVMTargetMachineRef t)
// {
// 	return unwrap(t)->getTargetLowering();
// }

char *LLVM_General_GetDefaultTargetTriple() {
  return strdup(sys::getDefaultTargetTriple().c_str());
}

char *LLVM_General_GetProcessTargetTriple() {
  return strdup(sys::getProcessTriple().c_str());
}

const char *LLVM_General_GetHostCPUName(size_t &len) {
  StringRef r = sys::getHostCPUName();
  len = r.size();
  return r.data();
}

char *LLVM_General_GetHostCPUFeatures() {
  StringMap<bool> featureMap;
  std::string features;
  if (sys::getHostCPUFeatures(featureMap)) {
    bool first = true;
    for (llvm::StringMap<bool>::const_iterator it = featureMap.begin();
         it != featureMap.end(); ++it) {
      if (!first) {
        features += ",";
      }
      first = false;
      features += (it->second ? "+" : "-") + it->first().str();
    }
  }
  return strdup(features.c_str());
}

char *LLVM_General_GetTargetMachineDataLayout(LLVMTargetMachineRef t) {
  return strdup(
      unwrap(t)->createDataLayout().getStringRepresentation().c_str());
}

inline LLVMTargetLibraryInfoRef wrap(const TargetLibraryInfoImpl *P) {
  TargetLibraryInfoImpl *X = const_cast<TargetLibraryInfoImpl*>(P);
  return reinterpret_cast<LLVMTargetLibraryInfoRef>(X);
}

LLVMTargetLibraryInfoRef
LLVM_General_CreateTargetLibraryInfo(const char *triple) {
    const TargetLibraryInfoImpl* p = new TargetLibraryInfoImpl(Triple(triple));
    return wrap(p);
}

LLVMBool LLVM_General_GetLibFunc(
	LLVMTargetLibraryInfoRef l,
	const char *funcName,
	LLVMLibFunc *f
) {
	LibFunc::Func func;
	LLVMBool result = unwrap(l)->getLibFunc(funcName, func);
	*f = wrap(func);
	return result;
}

const char *LLVM_General_LibFuncGetName(
	LLVMTargetLibraryInfoRef l,
	LLVMLibFunc f,
	size_t *nameSize
) {
	TargetLibraryInfo impl(*unwrap(l));
    StringRef s = impl.getName(unwrap(f));
	*nameSize = s.size();
	return s.data();
}

void LLVM_General_LibFuncSetAvailableWithName(
	LLVMTargetLibraryInfoRef l,
	LLVMLibFunc f,
	const char *name
) {
	unwrap(l)->setAvailableWithName(unwrap(f), name);
}

void LLVM_General_DisposeTargetLibraryInfo(LLVMTargetLibraryInfoRef l) {
	delete unwrap(l);
}

void LLVM_General_InitializeAllTargets() {
  InitializeAllTargetInfos();
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmPrinters();
  // None of the other components are bound yet
}

// This is identical to LLVMTargetMachineEmit but LLVM doesn’t expose this function so we copy it here.
LLVMBool LLVM_General_TargetMachineEmit(
    LLVMTargetMachineRef T,
    LLVMModuleRef M,
    raw_pwrite_stream &OS,
    LLVMCodeGenFileType codegen,
    char **ErrorMessage
) {
  TargetMachine* TM = unwrap(T);
  Module* Mod = unwrap(M);

  legacy::PassManager pass;

  std::string error;

  Mod->setDataLayout(TM->createDataLayout());

  TargetMachine::CodeGenFileType ft;
  switch (codegen) {
    case LLVMAssemblyFile:
      ft = TargetMachine::CGFT_AssemblyFile;
      break;
    default:
      ft = TargetMachine::CGFT_ObjectFile;
      break;
  }
  if (TM->addPassesToEmitFile(pass, OS, ft)) {
    error = "TargetMachine can't emit a file of this type";
    *ErrorMessage = strdup(error.c_str());
    return true;
  }

  pass.run(*Mod);

  OS.flush();
  return false;
}

}
