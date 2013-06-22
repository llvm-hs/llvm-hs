#define __STDC_LIMIT_MACROS
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Assembly/Parser.h"
#include "llvm/Assembly/PrintModulePass.h"
#include "llvm/Pass.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"

#include "llvm-c/Core.h"

using namespace llvm;

extern "C" {

LLVMModuleRef LLVM_General_GetModuleFromAssemblyInContext(
	LLVMContextRef context,
	const char *assembly,
	SMDiagnostic *error
) {
	return wrap(ParseAssemblyString(assembly, NULL, *error, *unwrap(context))); 
}

char *LLVM_General_GetModuleAssembly(LLVMModuleRef module) {
	std::string s;
	raw_string_ostream buf(s);
	ModulePass *printPass = createPrintModulePass(&buf);
	printPass->runOnModule(*unwrap(module));
	delete printPass;
	return strdup(buf.str().c_str());
}

}

