-- | Module to allow importing 'CallingConvention' distinctly qualified.
module LLVM.AST.CallingConvention where

import LLVM.Prelude

-- |  <http://llvm.org/docs/LangRef.html#callingconv>
data CallingConvention
  = C
  | Fast
  | Cold
  | GHC
  | HiPE
  | WebKit_JS
  | AnyReg
  | PreserveMost
  | PreserveAll
  | Swift
  | CXX_FastTLS
  | X86_StdCall
  | X86_FastCall
  | ARM_APCS
  | ARM_AAPCS
  | ARM_AAPCS_VFP
  | MSP430_INTR
  | X86_ThisCall
  | PTX_Kernel
  | PTX_Device
  | SPIR_FUNC
  | SPIR_KERNEL
  | Intel_OCL_BI
  | X86_64_SysV
  | Win64
  | X86_VectorCall
  | HHVM
  | HHVM_C
  | X86_Intr
  | AVR_Intr
  | AVR_Signal
  | AVR_Builtin
  | AMDGPU_VS
  | AMDGPU_HS
  | AMDGPU_GS
  | AMDGPU_PS
  | AMDGPU_CS
  | AMDGPU_Kernel
  | X86_RegCall
  | MSP430_Builtin
  | Numbered Word32
  deriving (Eq, Read, Show, Typeable, Data, Generic)

