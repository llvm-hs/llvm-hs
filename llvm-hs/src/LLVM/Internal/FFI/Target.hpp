#ifndef __LLVM_INTERNAL_FFI__TARGET__HPP__
#define __LLVM_INTERNAL_FFI__TARGET__HPP__

#include "llvm-c/TargetMachine.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {
DEFINE_SIMPLE_CONVERSION_FUNCTIONS(TargetMachine, LLVMTargetMachineRef)
}

#endif
