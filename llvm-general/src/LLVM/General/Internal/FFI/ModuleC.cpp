#define __STDC_LIMIT_MACROS
#include "llvm/IR/Module.h"
#include "llvm-c/Core.h"

using namespace llvm;

extern "C" {

char *LLVM_General_GetModuleIdentifier(LLVMModuleRef val) {
	return strdup(unwrap(val)->getModuleIdentifier().c_str());
}

LLVMValueRef LLVM_General_GetFirstAlias(LLVMModuleRef m) {
	Module *mod = unwrap(m);
	Module::alias_iterator i = mod->alias_begin();
	return i == mod->alias_end() ? 0 : wrap(i);
}

LLVMValueRef LLVM_General_GetNextAlias(LLVMValueRef a) {
	GlobalAlias *alias = unwrap<GlobalAlias>(a);
	Module::alias_iterator i = alias;
	if (++i == alias->getParent()->alias_end()) return 0;
	return wrap(i);
}

LLVMValueRef LLVM_General_JustAddAlias(LLVMModuleRef m, LLVMTypeRef ty, const char *name) {
	return wrap(new GlobalAlias(unwrap(ty), GlobalValue::ExternalLinkage, name, 0, unwrap(m)));
}

NamedMDNode *LLVM_General_GetOrAddNamedMetadata(LLVMModuleRef m, const char *name) {
	return unwrap(m)->getOrInsertNamedMetadata(name);
}

NamedMDNode *LLVM_General_GetFirstNamedMetadata(LLVMModuleRef m) {
	Module *mod = unwrap(m);
	Module::named_metadata_iterator i = mod->named_metadata_begin();
	return i == mod->named_metadata_end() ? 0 : i;
}

NamedMDNode *LLVM_General_GetNextNamedMetadata(NamedMDNode *a) {
	Module::named_metadata_iterator i = a;
	if (++i == a->getParent()->named_metadata_end()) return 0;
	return i;
}

void LLVM_General_ModuleAppendInlineAsm(LLVMModuleRef m, const char *s, unsigned l) {
	unwrap(m)->appendModuleInlineAsm(StringRef(s,l));
}

const char *LLVM_General_ModuleGetInlineAsm(LLVMModuleRef m) {
	return unwrap(m)->getModuleInlineAsm().c_str();
}

}
