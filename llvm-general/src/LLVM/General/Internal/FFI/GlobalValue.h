#ifndef __LLVM_GENERAL_INTERNAL_FFI__GLOBAL_VALUE__H__
#define __LLVM_GENERAL_INTERNAL_FFI__GLOBAL_VALUE__H__

#define LLVM_GENERAL_FOR_EACH_LINKAGE(macro)		\
	macro(External)																\
	macro(AvailableExternally)										\
	macro(LinkOnceAny)														\
	macro(LinkOnceODR)														\
	macro(WeakAny)																\
	macro(WeakODR)																\
	macro(Appending)															\
	macro(Internal)																\
	macro(Private)																\
	macro(ExternalWeak)														\
	macro(Common)

#define LLVM_GENERAL_FOR_EACH_VISIBILITY(macro)	\
	macro(Default)																\
	macro(Hidden)																	\
	macro(Protected)															\

#define LLVM_GENERAL_FOR_EACH_DLL_STORAGE_CLASS(macro)	\
	macro(Default)                                        \
	macro(DLLImport)                                      \
	macro(DLLExport)

#define LLVM_GENERAL_FOR_EACH_THREAD_LOCAL_MODE(macro)  \
	macro(NotThreadLocal)                                 \
	macro(GeneralDynamicTLSModel)                         \
	macro(LocalDynamicTLSModel)                           \
	macro(InitialExecTLSModel)                            \
	macro(LocalExecTLSModel)

#endif
