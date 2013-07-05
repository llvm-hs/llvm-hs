{-# LANGUAGE
  ForeignFunctionInterface,
  GeneralizedNewtypeDeriving
  #-}
module LLVM.General.Internal.FFI.Target where

import Foreign.Ptr
import Foreign.C

import LLVM.General.Internal.FFI.LLVMCTypes
import LLVM.General.Internal.FFI.MemoryBuffer
import LLVM.General.Internal.FFI.Module

data Target

foreign import ccall unsafe "LLVM_General_InitializeNativeTarget" initializeNativeTarget ::
    IO LLVMBool

foreign import ccall unsafe "LLVM_General_LookupTarget" lookupTarget ::
    CString -> CString -> Ptr CString -> Ptr CString -> IO (Ptr Target)

data TargetOptions

foreign import ccall unsafe "LLVM_General_CreateTargetOptions" createTargetOptions ::
  IO (Ptr TargetOptions)

foreign import ccall unsafe "LLVM_General_SetTargetOptionFlag" setTargetOptionFlag ::
  Ptr TargetOptions -> TargetOptionFlag -> LLVMBool -> IO ()

foreign import ccall unsafe "LLVM_General_GetTargetOptionFlag" getTargetOptionsFlag ::
  Ptr TargetOptions -> TargetOptionFlag -> IO LLVMBool

foreign import ccall unsafe "LLVM_General_SetStackAlignmentOverride" setStackAlignmentOverride ::
  Ptr TargetOptions -> CUInt -> IO ()

foreign import ccall unsafe "LLVM_General_GetStackAlignmentOverride" getStackAlignmentOverride ::
  Ptr TargetOptions -> IO CUInt

foreign import ccall unsafe "LLVM_General_SetTrapFuncName" setTrapFuncName ::
  Ptr TargetOptions -> CString -> IO ()

foreign import ccall unsafe "LLVM_General_GetTrapFuncName" getTrapFuncName ::
  Ptr TargetOptions -> IO CString

foreign import ccall unsafe "LLVM_General_SetFloatABIType" setFloatABIType ::
  Ptr TargetOptions -> FloatABIType -> IO ()

foreign import ccall unsafe "LLVM_General_GetFloatABIType" getFloatABIType ::
  Ptr TargetOptions -> IO FloatABIType

foreign import ccall unsafe "LLVM_General_SetAllowFPOpFusion" setAllowFPOpFusion ::
  Ptr TargetOptions -> FPOpFusionMode -> IO ()

foreign import ccall unsafe "LLVM_General_GetAllowFPOpFusion" getAllowFPOpFusion ::
  Ptr TargetOptions -> IO FPOpFusionMode

foreign import ccall unsafe "LLVM_General_SetSSPBufferSize" setSSPBufferSize ::
  Ptr TargetOptions -> CUInt -> IO ()

foreign import ccall unsafe "LLVM_General_GetSSPBufferSize" getSSPBufferSize ::
  Ptr TargetOptions -> IO CUInt

foreign import ccall unsafe "LLVM_General_DisposeTargetOptions" disposeTargetOptions ::
  Ptr TargetOptions -> IO ()

data TargetMachine

foreign import ccall unsafe "LLVM_General_CreateTargetMachine" createTargetMachine ::
  Ptr Target
  -> CString 
  -> CString
  -> CString
  -> Ptr TargetOptions
  -> RelocModel
  -> CodeModel
  -> CodeGenOptLevel
  -> IO (Ptr TargetMachine)

foreign import ccall unsafe "LLVMDisposeTargetMachine" disposeTargetMachine ::
  Ptr TargetMachine -> IO ()

foreign import ccall unsafe "LLVMTargetMachineEmitToFile" targetMachineEmitToFile ::
  Ptr TargetMachine -> Ptr Module -> CString -> CodeGenFileType -> Ptr CString -> IO LLVMBool

foreign import ccall unsafe "LLVMTargetMachineEmitToMemoryBuffer" targetMachineEmitToMemoryBuffer ::
  Ptr TargetMachine -> Ptr Module -> CodeGenFileType -> Ptr CString -> Ptr (Ptr MemoryBuffer) -> IO LLVMBool

data TargetLowering

foreign import ccall unsafe "LLVM_General_GetTargetLowering" getTargetLowering ::
  Ptr TargetMachine -> IO (Ptr TargetLowering)

foreign import ccall unsafe "LLVM_General_GetDefaultTargetTriple" getDefaultTargetTriple :: IO CString
foreign import ccall unsafe "LLVM_General_GetProcessTargetTriple" getProcessTargetTriple :: IO CString
foreign import ccall unsafe "LLVM_General_GetHostCPUName" getHostCPUName :: IO CString
foreign import ccall unsafe "LLVM_General_GetHostCPUFeatures" getHostCPUFeatures :: IO CString
