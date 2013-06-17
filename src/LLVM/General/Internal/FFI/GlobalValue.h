#ifndef __LLVM_GENERAL_INTERNAL_FFI__GLOBAL_VALUE__H__
#define __LLVM_GENERAL_INTERNAL_FFI__GLOBAL_VALUE__H__

#define LLVM_GENERAL_FOR_EACH_LINKAGE(macro)		\
	macro(External)																\
	macro(AvailableExternally)										\
	macro(LinkOnceAny)														\
	macro(LinkOnceODR)														\
	macro(LinkOnceODRAutoHide)										\
	macro(WeakAny)																\
	macro(WeakODR)																\
	macro(Appending)															\
	macro(Internal)																\
	macro(Private)																\
	macro(DLLImport)															\
	macro(DLLExport)															\
	macro(ExternalWeak)														\
	macro(Ghost)																	\
	macro(Common)																	\
	macro(LinkerPrivate)													\
	macro(LinkerPrivateWeak)											\


#define LLVM_GENERAL_FOR_EACH_VISIBILITY(macro)	\
	macro(Default)																\
	macro(Hidden)																	\
	macro(Protected)															\

#endif
