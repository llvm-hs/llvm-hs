#define __STDC_LIMIT_MACROS
#include "llvm/Support/CommandLine.h"

using namespace llvm;

extern "C" {

void HorribleHackToMimicLLVMVectorizerTestWithStateChangeableOnlyFromTheCommandLine() {
	char *argv[] = { "-bb-vectorize-ignore-target-info" };
	cl::ParseCommandLineOptions(sizeof(argv)/sizeof(argv[0]), argv);
}

}
