#define __STDC_LIMIT_MACROS
#include "llvm/Support/CommandLine.h"

using namespace llvm;

extern "C" {

void LLVM_General_ParseCommandLineOptions(unsigned argc, const char * const *argv, const char *overview) {
	cl::ParseCommandLineOptions(argc, argv, overview);
}

}
