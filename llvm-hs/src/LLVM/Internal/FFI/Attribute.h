#ifndef __LLVM_INTERNAL_FFI__ATTRIBUTES__H__
#define __LLVM_INTERNAL_FFI__ATTRIBUTES__H__


// The last three arguments are flags indicating if this is a
// parameter attribute, function result attribute or function attribute.
#define LLVM_HS_FOR_EACH_ATTRIBUTE_KIND(macro)	\
	macro(None,F,F,F)                                 \
	macro(Alignment,T,T,F)                            \
	macro(AllocSize,F,F,T)                            \
	macro(AlwaysInline,F,F,T)                         \
	macro(ArgMemOnly,F,F,T)                           \
	macro(Builtin,F,F,T)                              \
	macro(ByVal,T,F,F)                                \
	macro(Cold,F,F,T)                                 \
	macro(Convergent,F,F,T)                           \
	macro(Dereferenceable,T,T,F)                      \
	macro(DereferenceableOrNull,T,T,F)                \
    macro(ImmArg,T,F,F)                               \
	macro(InAlloca,T,F,F)                             \
	macro(InReg,T,T,F)                                \
	macro(InaccessibleMemOnly,F,F,T) \
	macro(InaccessibleMemOrArgMemOnly,F,F,T) \
	macro(InlineHint,F,F,T)                           \
	macro(JumpTable,F,F,T)                            \
	macro(MinSize,F,F,T)                              \
	macro(Naked,F,F,T)                                \
	macro(Nest,T,F,F)                                 \
	macro(NoAlias,T,T,F)                              \
	macro(NoBuiltin,F,F,T)                            \
	macro(NoCapture,T,F,F)                            \
    macro(NoCfCheck,F,F,T)                            \
	macro(NoDuplicate,F,F,T)                          \
	macro(NoFree,T,F,T)                               \
	macro(NoImplicitFloat,F,F,T)                      \
	macro(NoInline,F,F,T)                             \
	macro(NoRecurse,F,F,T)                            \
	macro(NoRedZone,F,F,T)                            \
	macro(NoReturn,F,F,T)                             \
	macro(NoSync,F,F,T)                               \
	macro(NoUnwind,F,F,T)                             \
	macro(NonLazyBind,F,F,T)                          \
	macro(NonNull,T,T,F)                              \
    macro(OptForFuzzing,F,F,T)                        \
	macro(OptimizeForSize,F,F,T)                      \
	macro(OptimizeNone,F,F,T)                         \
	macro(ReadNone,T,F,T)                             \
	macro(ReadOnly,T,F,T)                             \
	macro(Returned,T,F,F)                             \
	macro(ReturnsTwice,F,F,T)                         \
	macro(SExt,T,T,F)                                 \
	macro(SafeStack,F,F,T)                            \
	macro(SanitizeAddress,F,F,T)                      \
    macro(SanitizeHWAddress,F,F,T)                    \
    macro(SanitizeMemTag,F,F,T)                       \
	macro(SanitizeMemory,F,F,T)                       \
	macro(SanitizeThread,F,F,T)                       \
    macro(ShadowCallStack,F,F,T)                      \
    macro(Speculatable,F,F,T)                         \
    macro(SpeculativeLoadHardening,F,F,T)             \
	macro(StackAlignment,F,F,T)                       \
	macro(StackProtect,F,F,T)                         \
	macro(StackProtectReq,F,F,T)                      \
	macro(StackProtectStrong,F,F,T)                   \
    macro(StrictFP,F,F,T)                             \
	macro(StructRet,T,F,F)                            \
	macro(SwiftError,T,F,F)                           \
	macro(SwiftSelf,T,F,F)                            \
	macro(UWTable,F,F,T)                              \
	macro(WillReturn,F,F,T)                           \
	macro(WriteOnly,T,F,T)                            \
	macro(ZExt,T,T,F)                                 \
	macro(EndAttrKinds,F,F,F)

typedef enum {
#define ENUM_CASE(x,p,r,f) LLVM_Hs_AttributeKind_ ## x,
LLVM_HS_FOR_EACH_ATTRIBUTE_KIND(ENUM_CASE)
#undef ENUM_CASE
} LLVM_Hs_AttributeKind;

#endif
