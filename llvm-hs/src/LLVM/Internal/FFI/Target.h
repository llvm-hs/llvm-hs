#ifndef __LLVM_INTERNAL_FFI__TARGET__H__
#define __LLVM_INTERNAL_FFI__TARGET__H__

#define LLVM_HS_FOR_EACH_RELOC_MODEL(macro)	\
	macro(Default, Default)													\
	macro(Static, Static)														\
	macro(PIC, PIC_)																\
	macro(DynamicNoPic, DynamicNoPIC)

#define LLVM_HS_FOR_EACH_CODE_MODEL(macro) \
	macro(Default)																\
	macro(JITDefault)															\
	macro(Small)																	\
	macro(Kernel)																	\
	macro(Medium)																	\
	macro(Large)

#define LLVM_HS_FOR_EACH_CODE_GEN_OPT_LEVEL(macro) \
	macro(None)																						\
	macro(Less)																						\
	macro(Default)																				\
	macro(Aggressive)

#define LLVM_HS_FOR_EACH_CODE_GEN_FILE_TYPE(macro)	\
	macro(Assembly)                                       \
	macro(Object)

#define LLVM_HS_FOR_EACH_TARGET_OPTION_FLAG(macro)	\
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
#define ENUM_CASE(n) LLVM_Hs_TargetOptionFlag_ ## n,
	LLVM_HS_FOR_EACH_TARGET_OPTION_FLAG(ENUM_CASE)
#undef ENUM_CASE
} LLVM_Hs_TargetOptionFlag;

#define LLVM_HS_FOR_EACH_FLOAT_ABI(macro)	\
	macro(Default)																\
	macro(Soft)																		\
	macro(Hard)

typedef enum {
#define ENUM_CASE(n) LLVM_Hs_FloatABI_ ## n,
	LLVM_HS_FOR_EACH_FLOAT_ABI(ENUM_CASE)
#undef ENUM_CASE
} LLVM_Hs_FloatABI;

#define LLVM_HS_FOR_EACH_FP_OP_FUSION_MODE(macro)	\
	macro(Fast)																						\
	macro(Standard)																				\
	macro(Strict)

typedef enum {
#define ENUM_CASE(n) LLVM_Hs_FPOpFusionMode_ ## n,
	LLVM_HS_FOR_EACH_FP_OP_FUSION_MODE(ENUM_CASE)
#undef ENUM_CASE
} LLVM_Hs_FPOpFusionMode;

#endif
