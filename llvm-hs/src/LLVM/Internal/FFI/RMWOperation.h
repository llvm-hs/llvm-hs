#ifndef __LLVM_INTERNAL_FFI__RMW_OPERATION__H__
#define __LLVM_INTERNAL_FFI__RMW_OPERATION__H__

/* The AtomicRMWBinOp{FAdd,FSub} were missed from the C-API release of LLVM-9.
 * Add those manually here. This should be removed in the next release.
 *
 * https://github.com/llvm/llvm-project/commit/f57e968dd036b2230c59c00e1ed10fecf1668828#diff-ce8fe77b5ebb1f32a2f9665555fc5f5d
 */
typedef enum {
  LLVMAtomicRMWBinOp_Xchg,
  LLVMAtomicRMWBinOp_Add,
  LLVMAtomicRMWBinOp_Sub,
  LLVMAtomicRMWBinOp_And,
  LLVMAtomicRMWBinOp_Nand,
  LLVMAtomicRMWBinOp_Or,
  LLVMAtomicRMWBinOp_Xor,
  LLVMAtomicRMWBinOp_Max,
  LLVMAtomicRMWBinOp_Min,
  LLVMAtomicRMWBinOp_UMax,
  LLVMAtomicRMWBinOp_UMin,
  LLVMAtomicRMWBinOp_FAdd,
  LLVMAtomicRMWBinOp_FSub
} LLVMAtomicRMWBinOp_;

#endif
