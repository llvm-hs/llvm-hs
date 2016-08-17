module LLVM.General.Test.CallingConvention where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import LLVM.General.Test.Support

import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map

import LLVM.General.Context
import LLVM.General.Module
import LLVM.General.AST
import LLVM.General.AST.Type as T
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Global as G

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
       \declare" ++ (if name == "" then "" else (" " ++ name)) ++ " i32 @foo()\n")
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
   ("x86_64_win64cc", CC.X86_64_Win64)
  ]
 ]
