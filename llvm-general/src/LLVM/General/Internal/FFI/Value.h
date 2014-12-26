#ifndef __LLVM_GENERAL_INTERNAL_FFI__VALUE__H__
#define __LLVM_GENERAL_INTERNAL_FFI__VALUE__H__

#define LLVM_GENERAL_FOR_EACH_VALUE_SUBCLASS(macro) \
	macro(Argument) \
	macro(BasicBlock) \
	macro(Function) \
	macro(GlobalAlias) \
	macro(GlobalVariable) \
	macro(UndefValue) \
	macro(BlockAddress) \
	macro(ConstantExpr) \
	macro(ConstantAggregateZero) \
	macro(ConstantDataArray) \
	macro(ConstantDataVector) \
	macro(ConstantInt) \
	macro(ConstantFP) \
	macro(ConstantArray) \
	macro(ConstantStruct) \
	macro(ConstantVector) \
	macro(ConstantPointerNull) \
	macro(MDNode) \
	macro(MDString) \
	macro(InlineAsm) \
	macro(Instruction)

typedef enum {
#define ENUM_CASE(class) LLVM ## class ## SubclassId,
LLVM_GENERAL_FOR_EACH_VALUE_SUBCLASS(ENUM_CASE)
#undef ENUM_CASE
} LLVMValueSubclassId;

#endif
