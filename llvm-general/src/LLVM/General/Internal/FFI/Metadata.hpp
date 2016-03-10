#include "llvm/IR/Metadata.h"
#include "llvm-c/Core.h"

namespace llvm {
typedef struct LLVMOpaqueMetadata *LLVMMetadataRef;
DEFINE_ISA_CONVERSION_FUNCTIONS(Metadata, LLVMMetadataRef)
}
