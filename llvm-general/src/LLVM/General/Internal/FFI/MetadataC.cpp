#define __STDC_LIMIT_MACROS

#include <iostream>
#include "LLVM/General/Internal/FFI/Metadata.hpp"
#include "llvm/Support/FormattedStream.h"

#include "llvm/Config/llvm-config.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Metadata.h"
#include "llvm-c/Core.h"

using namespace llvm;

extern "C" {

LLVMMetadataRef LLVM_General_IsAMDString(LLVMMetadataRef md) {
    if (isa<MDString>(unwrap(md))) {
        return md;
    }
    return nullptr;
}

LLVMMetadataRef LLVM_General_MDStringInContext(LLVMContextRef c,
                                               const char *str, unsigned slen) {
    return wrap(MDString::get(*unwrap(c), StringRef(str, slen)));
}

const char *LLVM_General_GetMDString(LLVMMetadataRef md, unsigned* len) {
    if (const MDString *S = dyn_cast<MDString>(unwrap(md))) {
      *len = S->getString().size();
      return S->getString().data();
    }
    *len = 0;
    return nullptr;
}

LLVMMetadataRef LLVM_General_MDValue(LLVMValueRef v) {
    return wrap(ValueAsMetadata::get(unwrap(v)));
}

LLVMValueRef LLVM_General_MetadataOperand(LLVMContextRef c, LLVMMetadataRef md) {
    return wrap(MetadataAsValue::get(*unwrap(c), unwrap(md)));
}

LLVMMetadataRef LLVM_General_IsAMDNode(LLVMMetadataRef md) {
    if (isa<MDNode>(unwrap(md))) {
        return md;
    }
    return nullptr;
}

LLVMValueRef LLVM_General_GetMDValue(LLVMMetadataRef md) {
    return wrap(unwrap<ValueAsMetadata>(md)->getValue());
}

LLVMMetadataRef LLVM_General_GetMetadataOperand(LLVMValueRef val) {
    return wrap(unwrap<MetadataAsValue>(val)->getMetadata());
}

LLVMMetadataRef LLVM_General_MDNodeInContext(LLVMContextRef c,
                                             LLVMMetadataRef *mds,
                                             unsigned count) {
    return wrap(MDNode::get(*unwrap(c), ArrayRef<Metadata *>(unwrap(mds), count)));
}

LLVMMetadataRef LLVM_General_IsAMDValue(LLVMMetadataRef md) {
    if (isa<ValueAsMetadata>(unwrap(md))) {
        return md;
    }
    return nullptr;
}


LLVMValueRef LLVM_General_IsAMetadataOperand(LLVMValueRef val) {
    if (isa<MetadataAsValue>(unwrap(val))) {
        return val;
    }
    return nullptr;
}


unsigned LLVM_General_GetMDKindNames(
	LLVMContextRef c,
	const char **s,
	unsigned *l,
	unsigned n
) {
	SmallVector<StringRef, 8> ns;
	unwrap(c)->getMDKindNames(ns);
	if (ns.size() <= n) {
		for(unsigned i=0; i < ns.size(); ++i) {
			s[i] = ns[i].data();
			l[i] = ns[i].size();
		}
	}
	return ns.size();
}

unsigned LLVM_General_GetMDNodeNumOperands(LLVMMetadataRef v) {
	return unwrap<MDNode>(v)->getNumOperands();
}

void LLVM_General_NamedMetadataAddOperands(
	NamedMDNode *n,
	LLVMMetadataRef *ops,
	unsigned nOps
) {
	for(unsigned i = 0; i != nOps; ++i) n->addOperand(unwrap<MDNode>(ops[i]));
}

const char *LLVM_General_GetNamedMetadataName(
	NamedMDNode *n,
	unsigned *len
) {
	StringRef s = n->getName();
	*len = s.size();
	return s.data();
}

unsigned LLVM_General_GetNamedMetadataNumOperands(NamedMDNode *n) {
	return n->getNumOperands();
}

void LLVM_General_GetNamedMetadataOperands(NamedMDNode *n, LLVMMetadataRef *dest) {
	for(unsigned i = 0; i != n->getNumOperands(); ++i)
		dest[i] = wrap(n->getOperand(i));
}

LLVMMetadataRef LLVM_General_CreateTemporaryMDNodeInContext(LLVMContextRef c) {
	return wrap(MDNode::getTemporary(*unwrap(c), ArrayRef<Metadata *>()).release());
}

void LLVM_General_DestroyTemporaryMDNode(LLVMMetadataRef v) {
    MDNode::deleteTemporary(unwrap<MDNode>(v));
}

void LLVM_General_GetMDNodeOperands(LLVMMetadataRef md, LLVMMetadataRef *dest) {
    const auto *N = cast<MDNode>(unwrap(md));
    const unsigned numOperands = N->getNumOperands();
    for (unsigned i = 0; i < numOperands; i++)
        dest[i] = wrap(N->getOperand(i));
}

void LLVM_General_MetadataReplaceAllUsesWith(LLVMMetadataRef md, LLVMMetadataRef replacement) {
    auto *Node = unwrap<MDNode>(md);
    Node->replaceAllUsesWith(unwrap<Metadata>(replacement));
    MDNode::deleteTemporary(Node);
}

}

