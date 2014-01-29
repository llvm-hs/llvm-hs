#define __STDC_LIMIT_MACROS
#include "llvm/Support/raw_ostream.h"
#include "llvm-c/Core.h"

using namespace llvm;
using sys::fs::F_None;
using sys::fs::F_Excl;
using sys::fs::F_Binary;

extern "C" {

LLVMBool LLVM_General_WithFileRawOStream(
	const char *filename,
	LLVMBool excl,
	LLVMBool binary,
	const char *&error,
	void (&callback)(raw_ostream &ostream)
) {
	std::string e;
	raw_fd_ostream os(filename, e, (excl ? F_Excl : F_None) | (binary ? F_Binary : F_None));
	if (!e.empty()) {
		error = strdup(e.c_str());
		return false;
	}
	callback(os);
	return true;
}

void LLVM_General_WithBufferRawOStream(
	void (&outputCallback)(const char *start, size_t length),
	void (&streamCallback)(raw_ostream &ostream)
) {
	std::string s;
	{
		raw_string_ostream os(s);
		streamCallback(os);
	}
	outputCallback(s.data(), s.size());
}
	
}
