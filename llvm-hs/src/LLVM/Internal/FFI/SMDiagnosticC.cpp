#define __STDC_LIMIT_MACROS
#include "llvm/Config/llvm-config.h"
#include "llvm/IR/LLVMContext.h"

#include "llvm/Support/SourceMgr.h"
#include "llvm-c/Core.h"

#include "LLVM/Internal/FFI/SMDiagnostic.h"

using namespace llvm;

extern "C" {

SMDiagnostic *LLVM_Hs_CreateSMDiagnostic() { return new SMDiagnostic(); }
void LLVM_Hs_DisposeSMDiagnostic(SMDiagnostic *p) { delete p; }

LLVMDiagnosticKind LLVM_Hs_GetSMDiagnosticKind(SMDiagnostic *p) {
	switch(p->getKind()) {
#define ENUM_CASE(k) case SourceMgr::DK_ ## k: return LLVMDiagnosticKind ## k;
		LLVM_HS_FOR_EACH_DIAGNOSTIC_KIND(ENUM_CASE)
#undef ENUM_CASE
	default: return LLVMDiagnosticKind(0);
	}
}

int LLVM_Hs_GetSMDiagnosticLineNo(SMDiagnostic *p) { return p->getLineNo(); }
int LLVM_Hs_GetSMDiagnosticColumnNo(SMDiagnostic *p) { return p->getColumnNo(); }

const char *LLVM_Hs_GetSMDiagnosticFilename(SMDiagnostic *p, unsigned *len) { 
	*len = p->getFilename().size();
	return p->getFilename().data();
}
const char *LLVM_Hs_GetSMDiagnosticMessage(SMDiagnostic *p, unsigned *len) {
	*len = p->getMessage().size();
	return p->getMessage().data();
}
const char *LLVM_Hs_GetSMDiagnosticLineContents(SMDiagnostic *p, unsigned *len) {
	*len = p->getLineContents().size();
	return p->getLineContents().data();
}

}
