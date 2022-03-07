#define __STDC_LIMIT_MACROS
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"

#include "llvm-c/Core.h"
#include "LLVM/Internal/FFI/Value.h"
#include "LLVM/Internal/FFI/Constant.h"

using namespace llvm;

namespace llvm {

static const struct fltSemantics &unwrap(LLVMFloatSemantics s) {
    switch(s) {
#define ENUM_CASE(x) case LLVMFloatSemantics ## x: return APFloat::x();
LLVM_HS_FOR_EACH_FLOAT_SEMANTICS(ENUM_CASE)
#undef ENUM_CASE
    default: return APFloat::Bogus();
    }
}

}

extern "C" {

LLVMValueRef LLVM_Hs_GetConstantDataSequentialElementAsConstant(LLVMValueRef v, unsigned i) {
    return wrap(unwrap<ConstantDataSequential>(v)->getElementAsConstant(i));
}

LLVMValueRef LLVM_Hs_GetBlockAddressFunction(LLVMValueRef v) {
    return wrap(unwrap<BlockAddress>(v)->getFunction());
}

LLVMBasicBlockRef LLVM_Hs_GetBlockAddressBlock(LLVMValueRef v) {
    return wrap(unwrap<BlockAddress>(v)->getBasicBlock());
}

double LLVM_Hs_ConstFloatDoubleValue(LLVMValueRef v) {
    return unwrap<ConstantFP>(v)->getValueAPF().convertToDouble();
}

float LLVM_Hs_ConstFloatFloatValue(LLVMValueRef v) {
    return unwrap<ConstantFP>(v)->getValueAPF().convertToFloat();
}

LLVMValueRef LLVM_Hs_ConstCast(unsigned opcode, LLVMValueRef v, LLVMTypeRef t) {
    return wrap(ConstantExpr::getCast(opcode, unwrap<Constant>(v), unwrap(t)));
}

LLVMValueRef LLVM_Hs_ConstBinaryOperator(unsigned opcode, LLVMValueRef o0, LLVMValueRef o1) {
    return wrap(ConstantExpr::get(opcode, unwrap<Constant>(o0), unwrap<Constant>(o1)));
}

LLVMValueRef LLVM_Hs_ConstAdd(unsigned nsw, unsigned nuw, LLVMValueRef o0, LLVMValueRef o1) {
    return wrap(ConstantExpr::getAdd(unwrap<Constant>(o0), unwrap<Constant>(o1), nuw != 0, nsw != 0));
}

LLVMValueRef LLVM_Hs_ConstMul(unsigned nsw, unsigned nuw, LLVMValueRef o0, LLVMValueRef o1) {
    return wrap(ConstantExpr::getMul(unwrap<Constant>(o0), unwrap<Constant>(o1), nuw != 0, nsw != 0));
}

LLVMValueRef LLVM_Hs_ConstShl(unsigned nsw, unsigned nuw, LLVMValueRef o0, LLVMValueRef o1) {
    return wrap(ConstantExpr::getShl(unwrap<Constant>(o0), unwrap<Constant>(o1), nuw != 0, nsw != 0));
}

LLVMValueRef LLVM_Hs_ConstSub(unsigned nsw, unsigned nuw, LLVMValueRef o0, LLVMValueRef o1) {
    return wrap(ConstantExpr::getSub(unwrap<Constant>(o0), unwrap<Constant>(o1), nuw != 0, nsw != 0));
}

LLVMValueRef LLVM_Hs_ConstUDiv(unsigned isExact, LLVMValueRef o0, LLVMValueRef o1) {
    return wrap(ConstantExpr::getUDiv(unwrap<Constant>(o0), unwrap<Constant>(o1), isExact != 0));
}

LLVMValueRef LLVM_Hs_ConstSDiv(unsigned isExact, LLVMValueRef o0, LLVMValueRef o1) {
    return wrap(ConstantExpr::getSDiv(unwrap<Constant>(o0), unwrap<Constant>(o1), isExact != 0));
}

LLVMValueRef LLVM_Hs_ConstLShr(unsigned isExact, LLVMValueRef o0, LLVMValueRef o1) {
    return wrap(ConstantExpr::getLShr(unwrap<Constant>(o0), unwrap<Constant>(o1), isExact != 0));
}

LLVMValueRef LLVM_Hs_ConstAShr(unsigned isExact, LLVMValueRef o0, LLVMValueRef o1) {
    return wrap(ConstantExpr::getAShr(unwrap<Constant>(o0), unwrap<Constant>(o1), isExact != 0));
}

unsigned LLVM_Hs_GetConstCPPOpcode(LLVMValueRef v) {
    return unwrap<ConstantExpr>(v)->getOpcode();
}

unsigned LLVM_Hs_GetConstPredicate(LLVMValueRef v) {
    return unwrap<ConstantExpr>(v)->getPredicate();
}

const unsigned *LLVM_Hs_GetConstIndices(LLVMValueRef v, unsigned *n) {
    ArrayRef<unsigned> r = unwrap<ConstantExpr>(v)->getIndices();
    *n = r.size();
    return r.data();
}

const uint64_t *LLVM_Hs_GetConstantIntWords(LLVMValueRef v, unsigned *n) {
    const APInt &i = unwrap<ConstantInt>(v)->getValue();
    *n = i.getNumWords();
    return i.getRawData();
}

LLVMValueRef LLVM_Hs_ConstFloatOfArbitraryPrecision(
    LLVMContextRef c,
    unsigned bits,
    const uint64_t *words,
    LLVMFloatSemantics semantics
) {
    return wrap(
        ConstantFP::get(
            *unwrap(c),
            APFloat(unwrap(semantics), APInt(bits, ArrayRef<uint64_t>(words, (bits-1)/64 + 1)))
        )
    );
}

void LLVM_Hs_GetConstantFloatWords(LLVMValueRef v, uint64_t *bits) {
    APInt a = unwrap<ConstantFP>(v)->getValueAPF().bitcastToAPInt();
    for(unsigned i=0; i != a.getNumWords(); ++i) bits[i] = a.getRawData()[i];
}

LLVMValueRef LLVM_Hs_GetConstTokenNone(LLVMContextRef context) {
    return wrap(ConstantTokenNone::get(*unwrap(context)));
}


}
