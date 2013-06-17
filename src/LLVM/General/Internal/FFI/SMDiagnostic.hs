{-# LANGUAGE
  ForeignFunctionInterface,
  EmptyDataDecls
  #-}

-- | FFI functions for handling the LLVM SMDiagnostic class
module LLVM.General.Internal.FFI.SMDiagnostic where

import Foreign.Ptr
import Foreign.C

import LLVM.General.Internal.FFI.LLVMCTypes

data SMDiagnostic

-- | allocate an SMDiagnostic object
foreign import ccall unsafe "LLVM_General_CreateSMDiagnostic" createSMDiagnostic ::
  IO (Ptr SMDiagnostic)

foreign import ccall unsafe "LLVM_General_DisposeSMDiagnostic" disposeSMDiagnostic ::
  Ptr SMDiagnostic -> IO ()

foreign import ccall unsafe "LLVM_General_GetSMDiagnosticKind" getSMDiagnosticKind ::
  Ptr SMDiagnostic -> IO DiagnosticKind

foreign import ccall unsafe "LLVM_General_GetSMDiagnosticLineNo" getSMDiagnosticLineNo ::
  Ptr SMDiagnostic -> IO CInt 

foreign import ccall unsafe "LLVM_General_GetSMDiagnosticColumnNo" getSMDiagnosticColumnNo ::
  Ptr SMDiagnostic -> IO CInt 

foreign import ccall unsafe "LLVM_General_GetSMDiagnosticFilename" getSMDiagnosticFilename ::
  Ptr SMDiagnostic -> IO CString 

foreign import ccall unsafe "LLVM_General_GetSMDiagnosticMessage" getSMDiagnosticMessage ::
  Ptr SMDiagnostic -> IO CString 

foreign import ccall unsafe "LLVM_General_GetSMDiagnosticLineContents" getSMDiagnosticLineContents ::
  Ptr SMDiagnostic -> IO CString 

