{-# LANGUAGE
  MultiParamTypeClasses,
  TemplateHaskell,
  QuasiQuotes
  #-}
module LLVM.Internal.CallingConvention where

import LLVM.Prelude

import LLVM.Internal.Coding
import Foreign.C.Types (CUInt(..))

import qualified LLVM.Internal.FFI.LLVMCTypes as FFI
import LLVM.Internal.FFI.LLVMCTypes (callingConventionP)

import qualified LLVM.AST.CallingConvention as A.CC

instance Monad m => EncodeM m A.CC.CallingConvention FFI.CallingConvention where
  encodeM cc = return $ 
        case cc of
          A.CC.C -> FFI.callingConventionC
          A.CC.Fast -> FFI.callingConventionFast
          A.CC.Cold -> FFI.callingConventionCold
          A.CC.GHC -> FFI.callingConventionGHC
          A.CC.HiPE -> FFI.callingConventionHiPE
          A.CC.WebKit_JS -> FFI.callingConventionWebKit_JS
          A.CC.AnyReg -> FFI.callingConventionAnyReg
          A.CC.PreserveMost -> FFI.callingConventionPreserveMost
          A.CC.PreserveAll -> FFI.callingConventionPreserveAll
          A.CC.Swift -> FFI.callingConventionSwift
          A.CC.CXX_FastTLS -> FFI.callingConventionCXX_FAST_TLS
          A.CC.X86_StdCall -> FFI.callingConventionX86_StdCall
          A.CC.X86_FastCall -> FFI.callingConventionX86_FastCall
          A.CC.ARM_APCS -> FFI.callingConventionARM_APCS
          A.CC.ARM_AAPCS -> FFI.callingConventionARM_AAPCS
          A.CC.ARM_AAPCS_VFP -> FFI.callingConventionARM_AAPCS_VFP
          A.CC.MSP430_INTR -> FFI.callingConventionMSP430_INTR
          A.CC.X86_ThisCall -> FFI.callingConventionX86_ThisCall
          A.CC.PTX_Kernel -> FFI.callingConventionPTX_Kernel
          A.CC.PTX_Device -> FFI.callingConventionPTX_Device
          A.CC.SPIR_FUNC -> FFI.callingConventionSPIR_FUNC
          A.CC.SPIR_KERNEL -> FFI.callingConventionSPIR_KERNEL
          A.CC.Intel_OCL_BI -> FFI.callingConventionIntel_OCL_BI
          A.CC.X86_64_SysV -> FFI.callingConventionX86_64_SysV
          A.CC.Win64 -> FFI.callingConventionWin64
          A.CC.X86_VectorCall -> FFI.callingConventionX86_VectorCall
          A.CC.HHVM -> FFI.callingConventionHHVM
          A.CC.HHVM_C -> FFI.callingConventionHHVM_C
          A.CC.X86_Intr -> FFI.callingConventionX86_INTR
          A.CC.AVR_Intr -> FFI.callingConventionAVR_INTR
          A.CC.AVR_Signal -> FFI.callingConventionAVR_SIGNAL
          A.CC.AVR_Builtin -> FFI.callingConventionAVR_BUILTIN
          A.CC.AMDGPU_VS -> FFI.callingConventionAMDGPU_VS
          A.CC.AMDGPU_HS -> FFI.callingConventionAMDGPU_HS
          A.CC.AMDGPU_GS -> FFI.callingConventionAMDGPU_GS
          A.CC.AMDGPU_PS -> FFI.callingConventionAMDGPU_PS
          A.CC.AMDGPU_CS -> FFI.callingConventionAMDGPU_CS
          A.CC.AMDGPU_Kernel -> FFI.callingConventionAMDGPU_KERNEL
          A.CC.X86_RegCall -> FFI.callingConventionX86_RegCall
          A.CC.MSP430_Builtin -> FFI.callingConventionMSP430_BUILTIN
          A.CC.Numbered cc' -> FFI.CallingConvention (fromIntegral cc')

instance Monad m => DecodeM m A.CC.CallingConvention FFI.CallingConvention where
  decodeM cc = return $ case cc of
    [callingConventionP|C|] -> A.CC.C
    [callingConventionP|Fast|] -> A.CC.Fast
    [callingConventionP|Cold|] -> A.CC.Cold
    [callingConventionP|GHC|] -> A.CC.GHC
    [callingConventionP|HiPE|] -> A.CC.HiPE
    [callingConventionP|WebKit_JS|] -> A.CC.WebKit_JS
    [callingConventionP|AnyReg|] -> A.CC.AnyReg
    [callingConventionP|PreserveMost|] -> A.CC.PreserveMost
    [callingConventionP|PreserveAll|] -> A.CC.PreserveAll
    [callingConventionP|Swift|] -> A.CC.Swift
    [callingConventionP|CXX_FAST_TLS|] -> A.CC.CXX_FastTLS
    [callingConventionP|X86_StdCall|] -> A.CC.X86_StdCall
    [callingConventionP|X86_FastCall|] -> A.CC.X86_FastCall
    [callingConventionP|ARM_APCS|] -> A.CC.ARM_APCS
    [callingConventionP|ARM_AAPCS|] -> A.CC.ARM_AAPCS
    [callingConventionP|ARM_AAPCS_VFP|] -> A.CC.ARM_AAPCS_VFP
    [callingConventionP|MSP430_INTR|] -> A.CC.MSP430_INTR
    [callingConventionP|X86_ThisCall|] -> A.CC.X86_ThisCall
    [callingConventionP|PTX_Kernel|] -> A.CC.PTX_Kernel
    [callingConventionP|PTX_Device|] -> A.CC.PTX_Device
    [callingConventionP|SPIR_FUNC|] -> A.CC.SPIR_FUNC
    [callingConventionP|SPIR_KERNEL|] -> A.CC.SPIR_KERNEL
    [callingConventionP|Intel_OCL_BI|] -> A.CC.Intel_OCL_BI
    [callingConventionP|X86_64_SysV|] -> A.CC.X86_64_SysV
    [callingConventionP|Win64|] -> A.CC.Win64
    [callingConventionP|X86_VectorCall|] -> A.CC.X86_VectorCall
    [callingConventionP|HHVM|] -> A.CC.HHVM
    [callingConventionP|HHVM_C|] -> A.CC.HHVM_C
    [callingConventionP|X86_INTR|] -> A.CC.X86_Intr
    [callingConventionP|AVR_INTR|] -> A.CC.AVR_Intr
    [callingConventionP|AVR_SIGNAL|] -> A.CC.AVR_Signal
    [callingConventionP|AVR_BUILTIN|] -> A.CC.AVR_Builtin
    [callingConventionP|AMDGPU_VS|] -> A.CC.AMDGPU_VS
    [callingConventionP|AMDGPU_HS|] -> A.CC.AMDGPU_HS
    [callingConventionP|AMDGPU_GS|] -> A.CC.AMDGPU_GS
    [callingConventionP|AMDGPU_PS|] -> A.CC.AMDGPU_PS
    [callingConventionP|AMDGPU_CS|] -> A.CC.AMDGPU_CS
    [callingConventionP|AMDGPU_KERNEL|] -> A.CC.AMDGPU_Kernel
    [callingConventionP|X86_RegCall|] -> A.CC.X86_RegCall
    [callingConventionP|MSP430_BUILTIN|] -> A.CC.MSP430_Builtin
    FFI.CallingConvention (CUInt ci)
      | ci >= 64 -> A.CC.Numbered (fromIntegral ci)
      | otherwise -> error ("Unknown calling convention: " <> show ci)
