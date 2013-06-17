#ifndef __LLVM_GENERAL_INTERNAL_FFI__FUNCTION__H__
#define __LLVM_GENERAL_INTERNAL_FFI__FUNCTION__H__

#define LLVM_GENERAL_FOR_EACH_CALLCONV(macro) \
	macro(C)																		\
	macro(Fast)																	\
	macro(Cold)																	\

#define LLVM_GENERAL_FOR_EACH_PARAM_ATTR(macro)				 \
	macro(ZExt)																					 \
	macro(SExt)																					 \
	macro(InReg)																				 \
	macro(StructRet)																		 \
	macro(NoAlias)																			 \
	macro(ByVal)																				 \
	macro(NoCapture)																		 \
	macro(Nest)																					 \

#define LLVM_GENERAL_FOR_EACH_FUNCTION_ATTR(macro)	\
	macro(NoReturn,Attribute)													\
	macro(NoUnwind,Attribute)													\
	macro(ReadNone,Attribute)													\
	macro(ReadOnly,Attribute)													\
	macro(NoInline,Attribute)													\
	macro(AlwaysInline,Attribute)											\
	macro(OptimizeForSize,Attribute)									\
	macro(StackProtect,Attribute)											\
	macro(StackProtectReq,Attribute)									\
	macro(Alignment,)																	\
	macro(NoRedZone,Attribute)												\
	macro(NoImplicitFloat,Attribute)									\
	macro(Naked,Attribute)														\
	macro(InlineHint,Attribute)												\
	macro(StackAlignment,)														\
	macro(ReturnsTwice,)															\
	macro(UWTable,)																		\
	macro(NonLazyBind,)																\

#endif
