#define __STDC_LIMIT_MACROS
#include "llvm/Config/llvm-config.h"
#if LLVM_VERSION_MAJOR < 3 || (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR < 3)
#include "llvm/LLVMContext.h"
#include "llvm/Constants.h"
#else
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#endif

#include "llvm-c/Core.h"
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

const uint64_t *LLVM_General_GetConstantIntWords(LLVMValueRef v, unsigned *n) {
	const APInt &i = unwrap<ConstantInt>(v)->getValue();
	*n = i.getNumWords();
	return i.getRawData();
}

LLVMValueRef LLVM_General_ConstFloatOfArbitraryPrecision(
	LLVMContextRef c,
	unsigned bits,
	const uint64_t *words,
	unsigned notPairOfFloats
) {
#if LLVM_VERSION_MAJOR < 3 || (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR < 3)
#define OLD_WAY
#endif
#ifndef OLD_WAY
	const fltSemantics *sem = 0;
	switch(bits) {
	case 16: sem = &APFloat::IEEEhalf; break;
	case 32: sem = &APFloat::IEEEsingle; break;
	case 64: sem = &APFloat::IEEEdouble; break;
	case 128: sem = notPairOfFloats ? &APFloat::IEEEdouble : &APFloat::PPCDoubleDouble; break;
	case 80: sem = &APFloat::x87DoubleExtended; break;
	default: break;
	}
#endif
	APInt api(bits, ArrayRef<uint64_t>(words, (bits-1)/64 + 1));

	return wrap(
		ConstantFP::get(
			*unwrap(c),
#ifdef OLD_WAY
			APFloat(api, notPairOfFloats)
#else
			APFloat(*sem, api)
#endif
		)
	);
}

void LLVM_General_GetConstantFloatWords(LLVMValueRef v, uint64_t *bits) {
	APInt a = unwrap<ConstantFP>(v)->getValueAPF().bitcastToAPInt();
	for(unsigned i=0; i != a.getNumWords(); ++i) bits[i] = a.getRawData()[i];
}

}
