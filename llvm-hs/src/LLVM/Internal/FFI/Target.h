#ifndef __LLVM_INTERNAL_FFI__TARGET__H__
#define __LLVM_INTERNAL_FFI__TARGET__H__

#define LLVM_HS_FOR_EACH_RELOC_MODEL(macro) \
  macro(Default, Default)                         \
  macro(Static, Static)                           \
  macro(PIC, PIC_)                                \
  macro(DynamicNoPic, DynamicNoPIC)

#define LLVM_HS_FOR_EACH_CODE_MODEL(macro) \
  macro(Default)                                \
  macro(JITDefault)                             \
  macro(Small)                                  \
  macro(Kernel)                                 \
  macro(Medium)                                 \
  macro(Large)

#define LLVM_HS_FOR_EACH_CODE_GEN_OPT_LEVEL(macro) \
  macro(None)                                           \
  macro(Less)                                           \
  macro(Default)                                        \
  macro(Aggressive)

#define LLVM_HS_FOR_EACH_CODE_GEN_FILE_TYPE(macro)  \
  macro(Assembly)                                       \
  macro(Object)

#define LLVM_HS_FOR_EACH_TARGET_OPTION_FLAG(macro)  \
  macro(PrintMachineCode)                               \
  macro(UnsafeFPMath)                                   \
  macro(NoInfsFPMath)                                   \
  macro(NoNaNsFPMath)                                   \
  macro(NoTrappingFPMath)                               \
  macro(NoSignedZerosFPMath)                            \
  macro(HonorSignDependentRoundingFPMathOption)         \
  macro(NoZerosInBSS)                                   \
  macro(GuaranteedTailCallOpt)                          \
  macro(StackSymbolOrdering)                            \
  macro(EnableFastISel)                                 \
  macro(UseInitArray)                                   \
  macro(DisableIntegratedAS)                            \
  macro(RelaxELFRelocations)                            \
  macro(FunctionSections)                               \
  macro(DataSections)                                   \
  macro(UniqueSectionNames)                             \
  macro(TrapUnreachable)                                \
  macro(EmulatedTLS)                                    \
  macro(EnableIPRA)

typedef enum {
#define ENUM_CASE(n) LLVM_Hs_TargetOptionFlag_ ## n,
  LLVM_HS_FOR_EACH_TARGET_OPTION_FLAG(ENUM_CASE)
#undef ENUM_CASE
} LLVM_Hs_TargetOptionFlag;

#define LLVM_HS_FOR_EACH_MC_TARGET_OPTION_FLAG(macro)  \
  macro(MCRelaxAll)                                     \
  macro(MCNoExecStack)                                  \
  macro(MCFatalWarnings)                                \
  macro(MCNoWarn)                                       \
  macro(MCNoDeprecatedWarn)                             \
  macro(MCSaveTempLabels)                               \
  macro(MCUseDwarfDirectory)                            \
  macro(MCIncrementalLinkerCompatible)                  \
  macro(MCPIECopyRelocations)                           \
  macro(ShowMCEncoding)                                 \
  macro(ShowMCInst)                                     \
  macro(AsmVerbose)                                     \
  macro(PreserveAsmComments)

typedef enum {
#define ENUM_CASE(n) LLVM_Hs_MCTargetOptionFlag_ ## n,
  LLVM_HS_FOR_EACH_MC_TARGET_OPTION_FLAG(ENUM_CASE)
#undef ENUM_CASE
} LLVM_Hs_MCTargetOptionFlag;

#define LLVM_HS_FOR_EACH_DEBUG_COMPRESSION_TYPE(macro) \
    macro(None) \
    macro(GNU) \
    macro(Z)

typedef enum {
#define ENUM_CASE(n) LLVM_Hs_DebugCompressionType_ ## n,
    LLVM_HS_FOR_EACH_DEBUG_COMPRESSION_TYPE(ENUM_CASE)
#undef ENUM_CASE
} LLVM_Hs_DebugCompressionType;

#define LLVM_HS_FOR_EACH_FLOAT_ABI(macro) \
  macro(Default)                                \
  macro(Soft)                                   \
  macro(Hard)

typedef enum {
#define ENUM_CASE(n) LLVM_Hs_FloatABI_ ## n,
  LLVM_HS_FOR_EACH_FLOAT_ABI(ENUM_CASE)
#undef ENUM_CASE
} LLVM_Hs_FloatABI;

#define LLVM_HS_FOR_EACH_FP_OP_FUSION_MODE(macro) \
  macro(Fast)                                           \
  macro(Standard)                                       \
  macro(Strict)

typedef enum {
#define ENUM_CASE(n) LLVM_Hs_FPOpFusionMode_ ## n,
  LLVM_HS_FOR_EACH_FP_OP_FUSION_MODE(ENUM_CASE)
#undef ENUM_CASE
} LLVM_Hs_FPOpFusionMode;

#define LLVM_HS_FOR_EACH_THREAD_MODEL(macro) \
  macro(POSIX) \
  macro(Single)

typedef enum {
#define ENUM_CASE(n) LLVM_Hs_ThreadModel_ ## n,
  LLVM_HS_FOR_EACH_THREAD_MODEL(ENUM_CASE)
#undef ENUM_CASE
} LLVM_Hs_ThreadModel;

#define LLVM_HS_FOR_EACH_EABI(macro) \
  macro(Unknown)                                        \
  macro(Default)                                        \
  macro(EABI4)                                          \
  macro(EABI5)                                          \
  macro(GNU)

typedef enum {
#define ENUM_CASE(n) LLVM_Hs_EABI_ ## n,
  LLVM_HS_FOR_EACH_EABI(ENUM_CASE)
#undef ENUM_CASE
} LLVM_Hs_EABI;

#define LLVM_HS_FOR_EACH_DEBUGGER_KIND(macro) \
  macro(Default)                                        \
  macro(GDB)                                            \
  macro(LLDB)                                           \
  macro(SCE)

typedef enum {
#define ENUM_CASE(n) LLVM_Hs_DebuggerKind_ ## n,
  LLVM_HS_FOR_EACH_DEBUGGER_KIND(ENUM_CASE)
#undef ENUM_CASE
} LLVM_Hs_DebuggerKind;

#define LLVM_HS_FOR_EACH_FP_DENORMAL_MODE(macro) \
  macro(IEEE)                                           \
  macro(PreserveSign)                                   \
  macro(PositiveZero)

typedef enum {
#define ENUM_CASE(n) LLVM_Hs_FPDenormalMode_ ## n,
  LLVM_HS_FOR_EACH_FP_DENORMAL_MODE(ENUM_CASE)
#undef ENUM_CASE
} LLVM_Hs_FPDenormalMode;

#define LLVM_HS_FOR_EACH_EXCEPTION_HANDLING(macro) \
  macro(None)                                           \
  macro(DwarfCFI)                                       \
  macro(SjLj)                                           \
  macro(ARM)                                            \
  macro(WinEH)

typedef enum {
#define ENUM_CASE(n) LLVM_Hs_ExceptionHandling_ ## n,
  LLVM_HS_FOR_EACH_EXCEPTION_HANDLING(ENUM_CASE)
#undef ENUM_CASE
} LLVM_Hs_ExceptionHandling;

#endif
