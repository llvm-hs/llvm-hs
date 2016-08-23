#ifndef __LLVM_GENERAL_INTERNAL_FFI__TARGET__H__
#define __LLVM_GENERAL_INTERNAL_FFI__TARGET__H__

#define LLVM_GENERAL_FOR_EACH_RELOC_MODEL(macro)	\
	macro(Default, Default)													\
	macro(Static, Static)														\
	macro(PIC, PIC_)																\
	macro(DynamicNoPic, DynamicNoPIC)

#define LLVM_GENERAL_FOR_EACH_CODE_MODEL(macro) \
	macro(Default)																\
	macro(JITDefault)															\
	macro(Small)																	\
	macro(Kernel)																	\
	macro(Medium)																	\
	macro(Large)

#define LLVM_GENERAL_FOR_EACH_CODE_GEN_OPT_LEVEL(macro) \
	macro(None)																						\
	macro(Less)																						\
	macro(Default)																				\
	macro(Aggressive)

#define LLVM_GENERAL_FOR_EACH_CODE_GEN_FILE_TYPE(macro)	\
	macro(Assembly)                                       \
	macro(Object)

#define LLVM_GENERAL_FOR_EACH_TARGET_OPTION_FLAG(macro)	\
	macro(PrintMachineCode)																\
	macro(LessPreciseFPMADOption)													\
	macro(UnsafeFPMath)																		\
	macro(NoInfsFPMath)																		\
	macro(NoNaNsFPMath)																		\
	macro(HonorSignDependentRoundingFPMathOption)					\
	macro(NoZerosInBSS)																		\
	macro(GuaranteedTailCallOpt)													\
	macro(EnableFastISel)																	\
	macro(UseInitArray)																		\
	macro(DisableIntegratedAS)														\
	macro(CompressDebugSections)													\
	macro(TrapUnreachable)

typedef enum {
#define ENUM_CASE(n) LLVM_General_TargetOptionFlag_ ## n,
	LLVM_GENERAL_FOR_EACH_TARGET_OPTION_FLAG(ENUM_CASE)
#undef ENUM_CASE
} LLVM_General_TargetOptionFlag;

#define LLVM_GENERAL_FOR_EACH_FLOAT_ABI(macro)	\
	macro(Default)																\
	macro(Soft)																		\
	macro(Hard)

typedef enum {
#define ENUM_CASE(n) LLVM_General_FloatABI_ ## n,
	LLVM_GENERAL_FOR_EACH_FLOAT_ABI(ENUM_CASE)
#undef ENUM_CASE
} LLVM_General_FloatABI;

#define LLVM_GENERAL_FOR_EACH_FP_OP_FUSION_MODE(macro)	\
	macro(Fast)																						\
	macro(Standard)																				\
	macro(Strict)

typedef enum {
#define ENUM_CASE(n) LLVM_General_FPOpFusionMode_ ## n,
	LLVM_GENERAL_FOR_EACH_FP_OP_FUSION_MODE(ENUM_CASE)
#undef ENUM_CASE
} LLVM_General_FPOpFusionMode;

#endif
