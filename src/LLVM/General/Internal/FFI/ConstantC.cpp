#define __STDC_LIMIT_MACROS
#include "llvm-c/Core.h"
#include "llvm/Constants.h"
#include "LLVM/General/Internal/FFI/Value.h"

using namespace llvm;

extern "C" {

LLVMValueRef LLVM_General_GetConstantDataSequentialElementAsConstant(LLVMValueRef v, unsigned i) {
	wrap(unwrap<ConstantDataSequential>(v)->getElementAsConstant(i));
}

LLVMValueRef LLVM_General_GetBlockAddressFunction(LLVMValueRef v) {
	return wrap(unwrap<BlockAddress>(v)->getFunction());
}

LLVMBasicBlockRef LLVM_General_GetBlockAddressBlock(LLVMValueRef v) {
	return wrap(unwrap<BlockAddress>(v)->getBasicBlock());
}

double LLVM_General_ConstFloatDoubleValue(LLVMValueRef v) {
	return unwrap<ConstantFP>(v)->getValueAPF().convertToDouble();
}

float LLVM_General_ConstFloatFloatValue(LLVMValueRef v) {
	return unwrap<ConstantFP>(v)->getValueAPF().convertToFloat();
}

LLVMValueRef LLVM_General_ConstCast(unsigned opcode, LLVMValueRef v, LLVMTypeRef t) {
	return wrap(ConstantExpr::getCast(opcode, unwrap<Constant>(v), unwrap(t)));
}

LLVMValueRef LLVM_General_ConstBinaryOperator(unsigned opcode, LLVMValueRef o0, LLVMValueRef o1) {
	return wrap(ConstantExpr::get(opcode, unwrap<Constant>(o0), unwrap<Constant>(o1)));
}

unsigned LLVM_General_GetConstCPPOpcode(LLVMValueRef v) {
	return unwrap<ConstantExpr>(v)->getOpcode();
}

unsigned LLVM_General_GetConstPredicate(LLVMValueRef v) {
	return unwrap<ConstantExpr>(v)->getPredicate();
}

const unsigned *LLVM_General_GetConstIndices(LLVMValueRef v, unsigned *n) {
	ArrayRef<unsigned> r = unwrap<ConstantExpr>(v)->getIndices();
	*n = r.size();
	return r.data();
}

}
