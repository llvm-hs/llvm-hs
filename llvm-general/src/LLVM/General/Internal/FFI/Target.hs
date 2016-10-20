{-# LANGUAGE
  ForeignFunctionInterface,
  GeneralizedNewtypeDeriving
  #-}
module LLVM.General.Internal.FFI.Target where

import LLVM.General.Prelude

import Foreign.Ptr
import Foreign.C

import LLVM.General.Internal.FFI.LLVMCTypes
import LLVM.General.Internal.FFI.MemoryBuffer
import LLVM.General.Internal.FFI.Module
import LLVM.General.Internal.FFI.RawOStream
import LLVM.General.Internal.FFI.DataLayout

data Target

foreign import ccall unsafe "LLVM_General_InitializeNativeTarget" initializeNativeTarget ::
    IO LLVMBool

foreign import ccall unsafe "LLVM_General_LookupTarget" lookupTarget ::
    CString -> CString -> Ptr (OwnerTransfered CString) -> Ptr (OwnerTransfered CString) -> IO (Ptr Target)

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

foreign import ccall unsafe "LLVM_General_SetFloatABIType" setFloatABIType ::
  Ptr TargetOptions -> FloatABIType -> IO ()

foreign import ccall unsafe "LLVM_General_GetFloatABIType" getFloatABIType ::
  Ptr TargetOptions -> IO FloatABIType

foreign import ccall unsafe "LLVM_General_SetAllowFPOpFusion" setAllowFPOpFusion ::
  Ptr TargetOptions -> FPOpFusionMode -> IO ()

foreign import ccall unsafe "LLVM_General_GetAllowFPOpFusion" getAllowFPOpFusion ::
  Ptr TargetOptions -> IO FPOpFusionMode

foreign import ccall unsafe "LLVM_General_DisposeTargetOptions" disposeTargetOptions ::
  Ptr TargetOptions -> IO ()

data TargetMachine

foreign import ccall unsafe "LLVMCreateTargetMachine" createTargetMachine ::
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

foreign import ccall unsafe "LLVM_General_TargetMachineEmit" targetMachineEmit ::
  Ptr TargetMachine
  -> Ptr Module
  -> Ptr RawPWriteStream
  -> CodeGenFileType
  -> Ptr (OwnerTransfered CString)
  -> IO LLVMBool

foreign import ccall unsafe "LLVMTargetMachineEmitToFile" targetMachineEmitToFile ::
  Ptr TargetMachine
  -> Ptr Module
  -> CString
  -> CodeGenFileType
  -> Ptr (OwnerTransfered CString)
  -> IO LLVMBool

foreign import ccall unsafe "LLVMTargetMachineEmitToMemoryBuffer" targetMachineEmitToMemoryBuffer ::
  Ptr TargetMachine
  -> Ptr Module
  -> CodeGenFileType
  -> Ptr (OwnerTransfered CString)
  -> Ptr (Ptr MemoryBuffer)
  -> IO LLVMBool

data TargetLowering

-- foreign import ccall unsafe "LLVM_General_GetTargetLowering" getTargetLowering ::
--   Ptr TargetMachine -> IO (Ptr TargetLowering)

foreign import ccall unsafe "LLVMGetTargetMachineTriple" getTargetMachineTriple ::
  Ptr TargetMachine -> IO (OwnerTransfered CString)

foreign import ccall unsafe "LLVM_General_GetDefaultTargetTriple" getDefaultTargetTriple :: 
  IO (OwnerTransfered CString)

foreign import ccall unsafe "LLVM_General_GetProcessTargetTriple" getProcessTargetTriple :: 
  IO (OwnerTransfered CString)

foreign import ccall unsafe "LLVM_General_GetHostCPUName" getHostCPUName :: 
  Ptr CSize -> IO CString

foreign import ccall unsafe "LLVM_General_GetHostCPUFeatures" getHostCPUFeatures ::
  IO (OwnerTransfered CString)

foreign import ccall unsafe "LLVM_General_GetTargetMachineDataLayout" getTargetMachineDataLayout ::
  Ptr TargetMachine -> IO (OwnerTransfered CString)

data TargetLibraryInfo

foreign import ccall unsafe "LLVM_General_CreateTargetLibraryInfo" createTargetLibraryInfo ::
  CString -> IO (Ptr TargetLibraryInfo)

foreign import ccall unsafe "LLVM_General_GetLibFunc" getLibFunc ::
  Ptr TargetLibraryInfo -> CString -> Ptr LibFunc -> IO LLVMBool

foreign import ccall unsafe "LLVM_General_LibFuncGetName" libFuncGetName ::
  Ptr TargetLibraryInfo -> LibFunc -> Ptr CSize -> IO CString

foreign import ccall unsafe "LLVM_General_LibFuncSetAvailableWithName" libFuncSetAvailableWithName ::
  Ptr TargetLibraryInfo -> LibFunc -> CString -> IO ()

foreign import ccall unsafe "LLVM_General_DisposeTargetLibraryInfo" disposeTargetLibraryInfo ::
  Ptr TargetLibraryInfo -> IO ()

foreign import ccall unsafe "LLVM_General_InitializeAllTargets" initializeAllTargets :: IO ()

foreign import ccall unsafe "LLVMCreateTargetDataLayout" createTargetDataLayout ::
  Ptr TargetMachine -> IO (Ptr DataLayout)
