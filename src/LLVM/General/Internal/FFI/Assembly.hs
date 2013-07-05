{-# LANGUAGE
  ForeignFunctionInterface
  #-}

-- | Functions to read and write textual LLVM assembly
module LLVM.General.Internal.FFI.Assembly where

import LLVM.General.Internal.FFI.Context 
import LLVM.General.Internal.FFI.Module
import LLVM.General.Internal.FFI.SMDiagnostic
import LLVM.General.Internal.FFI.LLVMCTypes

import Foreign.C
import Foreign.Ptr

-- | Use LLVM's parser to parse a string of llvm assembly to get a module
foreign import ccall unsafe "LLVM_General_GetModuleFromAssemblyInContext" getModuleFromAssemblyInContext ::
    Ptr Context -> CString -> Ptr SMDiagnostic -> IO (Ptr Module)

-- | LLVM's serializer to generate a string of llvm assembly from a module
foreign import ccall unsafe "LLVM_General_GetModuleAssembly" getModuleAssembly ::
    Ptr Module -> IO MallocedCString



