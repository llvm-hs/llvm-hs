{-# LANGUAGE OverloadedStrings #-}
module LLVM.Test.CallingConvention where

import Test.Tasty
import Test.Tasty.HUnit

import LLVM.Test.Support

import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Map as Map

import LLVM.Context
import LLVM.Module
import LLVM.AST
import LLVM.AST.Type as T
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Global as G

import qualified Data.ByteString.Char8 as ByteString

tests = testGroup "CallingConvention" [
  testCase name $ strCheck (defaultModule {
    moduleDefinitions = [
     GlobalDefinition $ functionDefaults {
       G.returnType = i32,
       G.name = Name "foo",
       G.callingConvention = cc
     }
    ]
   }) ("; ModuleID = '<string>'\n\
       \source_filename = \"<string>\"\n\
       \\n\
       \declare" <> (if name == "" then "" else (" " <> ByteString.pack name)) <> " i32 @foo()\n")
   | (name, cc) <- [
   ("", CC.C),
   ("fastcc", CC.Fast),
   ("coldcc", CC.Cold),
   ("ghccc", CC.GHC),
   ("cc11", CC.HiPE),
   ("webkit_jscc", CC.WebKit_JS),
   ("anyregcc", CC.AnyReg),
   ("preserve_mostcc", CC.PreserveMost),
   ("preserve_allcc", CC.PreserveAll),
   ("swiftcc", CC.Swift),
   ("cxx_fast_tlscc", CC.CXX_FastTLS),
   ("x86_stdcallcc", CC.X86_StdCall),
   ("x86_fastcallcc", CC.X86_FastCall),
   ("arm_apcscc", CC.ARM_APCS),
   ("arm_aapcscc", CC.ARM_AAPCS),
   ("arm_aapcs_vfpcc", CC.ARM_AAPCS_VFP),
   ("msp430_intrcc", CC.MSP430_INTR),
   ("x86_thiscallcc", CC.X86_ThisCall),
   ("ptx_kernel", CC.PTX_Kernel),
   ("ptx_device", CC.PTX_Device),
   ("spir_func", CC.SPIR_FUNC),
   ("spir_kernel", CC.SPIR_KERNEL),
   ("intel_ocl_bicc", CC.Intel_OCL_BI),
   ("x86_64_sysvcc", CC.X86_64_SysV),
   ("win64cc", CC.Win64),
   ("x86_vectorcallcc", CC.X86_VectorCall),
   ("hhvmcc", CC.HHVM),
   ("hhvm_ccc", CC.HHVM_C),
   ("x86_intrcc", CC.X86_Intr),
   ("avr_intrcc ", CC.AVR_Intr), -- The spaces are necessary because there is a typo in LLVM’s printer
   ("avr_signalcc ", CC.AVR_Signal),
   ("cc86", CC.AVR_Builtin),
   ("amdgpu_vs", CC.AMDGPU_VS),
   ("amdgpu_hs", CC.AMDGPU_HS),
   ("amdgpu_gs", CC.AMDGPU_GS),
   ("amdgpu_ps", CC.AMDGPU_PS),
   ("amdgpu_cs", CC.AMDGPU_CS),
   ("amdgpu_kernel", CC.AMDGPU_Kernel),
   ("x86_regcallcc", CC.X86_RegCall),
   ("cc94", CC.MSP430_Builtin)
  ]
 ]
