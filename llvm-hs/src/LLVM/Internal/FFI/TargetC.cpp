#define __STDC_LIMIT_MACROS
#include "LLVM/Internal/FFI/ErrorHandling.hpp"
#include "LLVM/Internal/FFI/LibFunc.h"
#include "LLVM/Internal/FFI/Target.h"
#include "LLVM/Internal/FFI/Target.hpp"
#include "llvm-c/Core.h"
#include "llvm-c/Target.h"
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
#include "llvm/Target/CodeGenCWrappers.h"
#include "llvm/Target/TargetMachine.h"

using namespace llvm;

namespace llvm {
// Taken from llvm/lib/Target/TargetMachineC.cpp
// These functions need to be marked as static to avoid undefined behavior
// due to multiple definitions
static LLVMTargetRef wrap(const Target *P) {
    return reinterpret_cast<LLVMTargetRef>(const_cast<Target *>(P));
}

static Target *unwrap(LLVMTargetRef P) { return reinterpret_cast<Target *>(P); }

static inline TargetLibraryInfoImpl *unwrap(LLVMTargetLibraryInfoRef P) {
    return reinterpret_cast<TargetLibraryInfoImpl *>(P);
}

static inline LLVMTargetLibraryInfoRef wrap(const TargetLibraryInfoImpl *P) {
    TargetLibraryInfoImpl *X = const_cast<TargetLibraryInfoImpl *>(P);
    return reinterpret_cast<LLVMTargetLibraryInfoRef>(X);
}

static FloatABI::ABIType unwrap(LLVM_Hs_FloatABI x) {
    switch (x) {
#define ENUM_CASE(x)                                                           \
    case LLVM_Hs_FloatABI_##x:                                                 \
        return FloatABI::x;
        LLVM_HS_FOR_EACH_FLOAT_ABI(ENUM_CASE)
#undef ENUM_CASE
    default:
        return FloatABI::ABIType(0);
    }
}

static LibFunc unwrap(LLVMLibFunc x) {
    switch (x) {
#define ENUM_CASE(x)                                                           \
    case LLVMLibFunc__##x:                                                     \
        return LibFunc_##x;
        LLVM_HS_FOR_EACH_LIB_FUNC(ENUM_CASE)
#undef ENUM_CASE
    default:
        return LibFunc(0);
    }
}

static LLVMLibFunc wrap(LibFunc x) {
    switch (x) {
#define ENUM_CASE(x)                                                           \
    case LibFunc_##x:                                                          \
        return LLVMLibFunc__##x;
        LLVM_HS_FOR_EACH_LIB_FUNC(ENUM_CASE)
#undef ENUM_CASE
    default:
        return LLVMLibFunc(0);
    }
}

static LLVM_Hs_FloatABI wrap(FloatABI::ABIType x) {
    switch (x) {
#define ENUM_CASE(x)                                                           \
    case FloatABI::x:                                                          \
        return LLVM_Hs_FloatABI_##x;
        LLVM_HS_FOR_EACH_FLOAT_ABI(ENUM_CASE)
#undef ENUM_CASE
    default:
        return LLVM_Hs_FloatABI(0);
    }
}

static FPOpFusion::FPOpFusionMode unwrap(LLVM_Hs_FPOpFusionMode x) {
    switch (x) {
#define ENUM_CASE(x)                                                           \
    case LLVM_Hs_FPOpFusionMode_##x:                                           \
        return FPOpFusion::x;
        LLVM_HS_FOR_EACH_FP_OP_FUSION_MODE(ENUM_CASE)
#undef ENUM_CASE
    default:
        return FPOpFusion::FPOpFusionMode(0);
    }
}

static LLVM_Hs_FPOpFusionMode wrap(FPOpFusion::FPOpFusionMode x) {
    switch (x) {
#define ENUM_CASE(x)                                                           \
    case FPOpFusion::x:                                                        \
        return LLVM_Hs_FPOpFusionMode_##x;
        LLVM_HS_FOR_EACH_FP_OP_FUSION_MODE(ENUM_CASE)
#undef ENUM_CASE
    default:
        return LLVM_Hs_FPOpFusionMode(0);
    }
}

static ThreadModel::Model unwrap(LLVM_Hs_ThreadModel x) {
    switch (x) {
#define ENUM_CASE(x)                                                           \
    case LLVM_Hs_ThreadModel_##x:                                              \
        return ThreadModel::x;
        LLVM_HS_FOR_EACH_THREAD_MODEL(ENUM_CASE)
#undef ENUM_CASE
    default:
        return ThreadModel::Model(0);
    }
}

static LLVM_Hs_ThreadModel wrap(ThreadModel::Model x) {
    switch (x) {
#define ENUM_CASE(x)                                                           \
    case ThreadModel::x:                                                       \
        return LLVM_Hs_ThreadModel_##x;
        LLVM_HS_FOR_EACH_THREAD_MODEL(ENUM_CASE)
#undef ENUM_CASE
    default:
        return LLVM_Hs_ThreadModel(0);
    }
}
static EABI unwrap(LLVM_Hs_EABI x) {
    switch (x) {
#define ENUM_CASE(x)                                                           \
    case LLVM_Hs_EABI_##x:                                                     \
        return EABI::x;
        LLVM_HS_FOR_EACH_EABI(ENUM_CASE)
#undef ENUM_CASE
    default:
        return EABI(0);
    }
}

static LLVM_Hs_EABI wrap(EABI x) {
    switch (x) {
#define ENUM_CASE(x)                                                           \
    case EABI::x:                                                              \
        return LLVM_Hs_EABI_##x;
        LLVM_HS_FOR_EACH_EABI(ENUM_CASE)
#undef ENUM_CASE
    default:
        return LLVM_Hs_EABI(0);
    }
}

static DebuggerKind unwrap(LLVM_Hs_DebuggerKind x) {
    switch (x) {
#define ENUM_CASE(x)                                                           \
    case LLVM_Hs_DebuggerKind_##x:                                             \
        return DebuggerKind::x;
        LLVM_HS_FOR_EACH_DEBUGGER_KIND(ENUM_CASE)
#undef ENUM_CASE
    default:
        return DebuggerKind(0);
    }
}

static LLVM_Hs_DebuggerKind wrap(DebuggerKind x) {
    switch (x) {
#define ENUM_CASE(x)                                                           \
    case DebuggerKind::x:                                                      \
        return LLVM_Hs_DebuggerKind_##x;
        LLVM_HS_FOR_EACH_DEBUGGER_KIND(ENUM_CASE)
#undef ENUM_CASE
    default:
        return LLVM_Hs_DebuggerKind(0);
    }
}

static FPDenormal::DenormalMode unwrap(LLVM_Hs_FPDenormalMode x) {
    switch (x) {
#define ENUM_CASE(x)                                                           \
    case LLVM_Hs_FPDenormalMode_##x:                                           \
        return FPDenormal::x;
        LLVM_HS_FOR_EACH_FP_DENORMAL_MODE(ENUM_CASE)
#undef ENUM_CASE
    default:
        return FPDenormal::DenormalMode(0);
    }
}

static LLVM_Hs_FPDenormalMode wrap(FPDenormal::DenormalMode x) {
    switch (x) {
#define ENUM_CASE(x)                                                           \
    case FPDenormal::x:                                                        \
        return LLVM_Hs_FPDenormalMode_##x;
        LLVM_HS_FOR_EACH_FP_DENORMAL_MODE(ENUM_CASE)
#undef ENUM_CASE
    default:
        return LLVM_Hs_FPDenormalMode(0);
    }
}
static ExceptionHandling unwrap(LLVM_Hs_ExceptionHandling x) {
    switch (x) {
#define ENUM_CASE(x)                                                           \
    case LLVM_Hs_ExceptionHandling_##x:                                        \
        return ExceptionHandling::x;
        LLVM_HS_FOR_EACH_EXCEPTION_HANDLING(ENUM_CASE)
#undef ENUM_CASE
    default:
        return ExceptionHandling(0);
    }
}

static LLVM_Hs_ExceptionHandling wrap(ExceptionHandling x) {
    switch (x) {
#define ENUM_CASE(x)                                                           \
    case ExceptionHandling::x:                                                 \
        return LLVM_Hs_ExceptionHandling_##x;
        LLVM_HS_FOR_EACH_EXCEPTION_HANDLING(ENUM_CASE)
#undef ENUM_CASE
    default:
        return LLVM_Hs_ExceptionHandling(0);
    }
}

} // namespace llvm

extern "C" {

LLVMBool LLVM_Hs_InitializeNativeTarget() {
    return LLVMInitializeNativeTarget() || InitializeNativeTargetAsmPrinter() ||
           InitializeNativeTargetAsmParser();
}

LLVMTargetRef LLVM_Hs_LookupTarget(const char *arch, const char *ctriple,
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

TargetOptions *LLVM_Hs_CreateTargetOptions() {
    TargetOptions *to = new TargetOptions();
    return to;
}

void LLVM_Hs_SetTargetOptionFlag(TargetOptions *to, LLVM_Hs_TargetOptionFlag f,
                                 unsigned v) {
    switch (f) {
#define ENUM_CASE(op)                                                          \
    case LLVM_Hs_TargetOptionFlag_##op:                                        \
        to->op = v ? 1 : 0;                                                    \
        break;
        LLVM_HS_FOR_EACH_TARGET_OPTION_FLAG(ENUM_CASE)
#undef ENUM_CASE
    }
}

void LLVM_Hs_SetMCTargetOptionFlag(MCTargetOptions *to,
                                   LLVM_Hs_MCTargetOptionFlag f, unsigned v) {
    switch (f) {
#define ENUM_CASE(op)                                                          \
    case LLVM_Hs_MCTargetOptionFlag_##op:                                      \
        to->op = v ? 1 : 0;                                                    \
        break;
        LLVM_HS_FOR_EACH_MC_TARGET_OPTION_FLAG(ENUM_CASE)
#undef ENUM_CASE
    }
}

static llvm::DebugCompressionType
unwrap(LLVM_Hs_DebugCompressionType compressionType) {
    switch (compressionType) {
#define ENUM_CASE(op)                                                          \
    case LLVM_Hs_DebugCompressionType_##op:                                    \
        return llvm::DebugCompressionType::op;
        LLVM_HS_FOR_EACH_DEBUG_COMPRESSION_TYPE(ENUM_CASE)
#undef ENUM_CASE
    default:
        reportFatalError("Unknown debug compression type");
        return llvm::DebugCompressionType::None;
    }
}

static LLVM_Hs_DebugCompressionType
wrap(llvm::DebugCompressionType compressionType) {
    switch (compressionType) {
#define ENUM_CASE(op)                                                          \
    case llvm::DebugCompressionType::op:                                       \
        return LLVM_Hs_DebugCompressionType_##op;
        LLVM_HS_FOR_EACH_DEBUG_COMPRESSION_TYPE(ENUM_CASE)
#undef ENUM_CASE
    default: {
        reportFatalError("Unknown debug compression type");
        return LLVM_Hs_DebugCompressionType_None;
    }
    }
}

void LLVM_Hs_SetCompressDebugSections(TargetOptions *to,
                                      LLVM_Hs_DebugCompressionType compress) {
    to->CompressDebugSections = unwrap(compress);
}

LLVM_Hs_DebugCompressionType
LLVM_Hs_GetCompressDebugSections(TargetOptions *to) {
    return wrap(to->CompressDebugSections);
}

unsigned LLVM_Hs_GetTargetOptionFlag(TargetOptions *to,
                                     LLVM_Hs_TargetOptionFlag f) {
    switch (f) {
#define ENUM_CASE(op)                                                          \
    case LLVM_Hs_TargetOptionFlag_##op:                                        \
        return to->op;
        LLVM_HS_FOR_EACH_TARGET_OPTION_FLAG(ENUM_CASE)
#undef ENUM_CASE
    default:
        reportFatalError("Unknown target option flag");
        return 0;
    }
}

unsigned LLVM_Hs_GetMCTargetOptionFlag(MCTargetOptions *to,
                                       LLVM_Hs_MCTargetOptionFlag f) {
    switch (f) {
#define ENUM_CASE(op)                                                          \
    case LLVM_Hs_MCTargetOptionFlag_##op:                                      \
        return to->op;
        LLVM_HS_FOR_EACH_MC_TARGET_OPTION_FLAG(ENUM_CASE)
#undef ENUM_CASE
    default:
        reportFatalError("Unknown machine code target option flag");
        return 0;
    }
}

void LLVM_Hs_SetStackAlignmentOverride(TargetOptions *to, unsigned v) {
    to->StackAlignmentOverride = v;
}

unsigned LLVM_Hs_GetStackAlignmentOverride(TargetOptions *to) {
    return to->StackAlignmentOverride;
}

void LLVM_Hs_SetFloatABIType(TargetOptions *to, LLVM_Hs_FloatABI v) {
    to->FloatABIType = unwrap(v);
}

LLVM_Hs_FloatABI LLVM_Hs_GetFloatABIType(TargetOptions *to) {
    return wrap(to->FloatABIType);
}

void LLVM_Hs_SetAllowFPOpFusion(TargetOptions *to, LLVM_Hs_FPOpFusionMode v) {
    to->AllowFPOpFusion = unwrap(v);
}

LLVM_Hs_FPOpFusionMode LLVM_Hs_GetAllowFPOpFusion(TargetOptions *to) {
    return wrap(to->AllowFPOpFusion);
}

void LLVM_Hs_SetThreadModel(TargetOptions *to, LLVM_Hs_ThreadModel v) {
    to->ThreadModel = unwrap(v);
}

LLVM_Hs_ThreadModel LLVM_Hs_GetThreadModel(TargetOptions *to) {
    return wrap(to->ThreadModel);
}

void LLVM_Hs_SetEABIVersion(TargetOptions *to, LLVM_Hs_EABI v) {
    to->EABIVersion = unwrap(v);
}

LLVM_Hs_EABI LLVM_Hs_GetEABIVersion(TargetOptions *to) {
    return wrap(to->EABIVersion);
}

void LLVM_Hs_SetDebuggerTuning(TargetOptions *to, LLVM_Hs_DebuggerKind v) {
    to->DebuggerTuning = unwrap(v);
}

LLVM_Hs_DebuggerKind LLVM_Hs_GetDebuggerTuning(TargetOptions *to) {
    return wrap(to->DebuggerTuning);
}

void LLVM_Hs_SetFPDenormalMode(TargetOptions *to, LLVM_Hs_FPDenormalMode v) {
    to->FPDenormalMode = unwrap(v);
}

LLVM_Hs_FPDenormalMode LLVM_Hs_GetFPDenormalMode(TargetOptions *to) {
    return wrap(to->FPDenormalMode);
}

void LLVM_Hs_SetExceptionModel(TargetOptions *to, LLVM_Hs_ExceptionHandling v) {
    to->ExceptionModel = unwrap(v);
}

LLVM_Hs_ExceptionHandling LLVM_Hs_GetExceptionModel(TargetOptions *to) {
    return wrap(to->ExceptionModel);
}

void LLVM_Hs_DisposeTargetOptions(TargetOptions *t) { delete t; }

// const TargetLowering *LLVM_Hs_GetTargetLowering(LLVMTargetMachineRef t)
// {
// 	return unwrap(t)->getTargetLowering();
// }

char *LLVM_Hs_GetDefaultTargetTriple() {
    return strdup(sys::getDefaultTargetTriple().c_str());
}

char *LLVM_Hs_GetProcessTargetTriple() {
    return strdup(sys::getProcessTriple().c_str());
}

const char *LLVM_Hs_GetHostCPUName(size_t &len) {
    StringRef r = sys::getHostCPUName();
    len = r.size();
    return r.data();
}

char *LLVM_Hs_GetHostCPUFeatures() {
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

char *LLVM_Hs_GetTargetMachineDataLayout(LLVMTargetMachineRef t) {
    return strdup(
        unwrap(t)->createDataLayout().getStringRepresentation().c_str());
}

LLVMTargetLibraryInfoRef LLVM_Hs_CreateTargetLibraryInfo(const char *triple) {
    const TargetLibraryInfoImpl *p = new TargetLibraryInfoImpl(Triple(triple));
    return wrap(p);
}

LLVMBool LLVM_Hs_GetLibFunc(LLVMTargetLibraryInfoRef l, const char *funcName,
                            LLVMLibFunc *f) {
    LibFunc func;
    LLVMBool result = unwrap(l)->getLibFunc(funcName, func);
    *f = wrap(func);
    return result;
}

const char *LLVM_Hs_LibFuncGetName(LLVMTargetLibraryInfoRef l, LLVMLibFunc f,
                                   size_t *nameSize) {
    TargetLibraryInfo impl(*unwrap(l));
    StringRef s = impl.getName(unwrap(f));
    *nameSize = s.size();
    return s.data();
}

void LLVM_Hs_LibFuncSetAvailableWithName(LLVMTargetLibraryInfoRef l,
                                         LLVMLibFunc f, const char *name) {
    unwrap(l)->setAvailableWithName(unwrap(f), name);
}

void LLVM_Hs_DisposeTargetLibraryInfo(LLVMTargetLibraryInfoRef l) {
    delete unwrap(l);
}

void LLVM_Hs_InitializeAllTargets() {
    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmPrinters();
    // None of the other components are bound yet
}

LLVMTargetMachineRef LLVM_Hs_CreateTargetMachine(
    LLVMTargetRef T, const char *Triple, const char *CPU, const char *Features,
    TargetOptions *TO, LLVMRelocMode Reloc, LLVMCodeModel CodeModel,
    LLVMCodeGenOptLevel Level) {
    Optional<Reloc::Model> RM;
    switch (Reloc) {
    case LLVMRelocStatic:
        RM = Reloc::Static;
        break;
    case LLVMRelocPIC:
        RM = Reloc::PIC_;
        break;
    case LLVMRelocDynamicNoPic:
        RM = Reloc::DynamicNoPIC;
        break;
    default:
        break;
    }

    bool JIT;
    Optional<CodeModel::Model> CM = unwrap(CodeModel, JIT);

    CodeGenOpt::Level OL;
    switch (Level) {
    case LLVMCodeGenLevelNone:
        OL = CodeGenOpt::None;
        break;
    case LLVMCodeGenLevelLess:
        OL = CodeGenOpt::Less;
        break;
    case LLVMCodeGenLevelAggressive:
        OL = CodeGenOpt::Aggressive;
        break;
    default:
        OL = CodeGenOpt::Default;
        break;
    }

    return wrap(unwrap(T)->createTargetMachine(Triple, CPU, Features, *TO, RM,
                                               CM, OL, JIT));
}

TargetOptions *LLVM_Hs_TargetMachineOptions(LLVMTargetMachineRef TM) {
    return &unwrap(TM)->Options;
}

MCTargetOptions *LLVM_Hs_MCTargetOptions(TargetOptions *to) {
    return &to->MCOptions;
}

// This is identical to LLVMTargetMachineEmit but LLVM doesn’t expose this
// function so we copy it here.
LLVMBool LLVM_Hs_TargetMachineEmit(LLVMTargetMachineRef T, LLVMModuleRef M,
                                   raw_pwrite_stream *OS,
                                   LLVMCodeGenFileType codegen,
                                   char **ErrorMessage) {
    TargetMachine *TM = unwrap(T);
    Module *Mod = unwrap(M);

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
    if (TM->addPassesToEmitFile(pass, *OS, nullptr, ft)) {
        error = "TargetMachine can't emit a file of this type";
        *ErrorMessage = strdup(error.c_str());
        return true;
    }

    pass.run(*Mod);

    OS->flush();
    return false;
}
}
