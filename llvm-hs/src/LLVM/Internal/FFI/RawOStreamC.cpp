#define __STDC_LIMIT_MACROS
#include "llvm-c/Core.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

extern "C" {

LLVMBool
LLVM_Hs_WithFileRawPWriteStream(const char *filename, LLVMBool excl,
                                LLVMBool text, char **error,
                                void (&callback)(raw_pwrite_stream &ostream)) {
    std::error_code e;
    raw_fd_ostream os(filename, e,
                      excl ? sys::fs::CD_CreateNew : sys::fs::CD_OpenAlways,
                      sys::fs::FA_Write,
                      text ? sys::fs::OF_Text : sys::fs::OF_None);
    if (e) {
        *error = strdup(e.message().c_str());
        return false;
    }
    callback(os);
    return true;
}

void LLVM_Hs_WithBufferRawPWriteStream(
    void (&outputCallback)(const char *start, size_t length),
    void (&streamCallback)(raw_pwrite_stream &ostream)) {
    SmallString<0> s;
    raw_svector_ostream os(s);
    streamCallback(os);
    outputCallback(os.str().data(), s.size());
}
}
