{-# LANGUAGE
  ForeignFunctionInterface
  #-}
-- | FFI functions for handling the LLVM SMDiagnostic class
module LLVM.Internal.FFI.SMDiagnostic where

import LLVM.Prelude

import Foreign.Ptr
import Foreign.C

import LLVM.Internal.FFI.LLVMCTypes

data SMDiagnostic

-- | allocate an SMDiagnostic object
foreign import ccall unsafe "LLVM_Hs_CreateSMDiagnostic" createSMDiagnostic ::
  IO (Ptr SMDiagnostic)

foreign import ccall unsafe "LLVM_Hs_DisposeSMDiagnostic" disposeSMDiagnostic ::
  Ptr SMDiagnostic -> IO ()

foreign import ccall unsafe "LLVM_Hs_GetSMDiagnosticKind" getSMDiagnosticKind ::
  Ptr SMDiagnostic -> IO DiagnosticKind

foreign import ccall unsafe "LLVM_Hs_GetSMDiagnosticLineNo" getSMDiagnosticLineNo ::
  Ptr SMDiagnostic -> IO CInt 

foreign import ccall unsafe "LLVM_Hs_GetSMDiagnosticColumnNo" getSMDiagnosticColumnNo ::
  Ptr SMDiagnostic -> IO CInt 

foreign import ccall unsafe "LLVM_Hs_GetSMDiagnosticFilename" getSMDiagnosticFilename ::
  Ptr SMDiagnostic -> Ptr CUInt -> IO CString 

foreign import ccall unsafe "LLVM_Hs_GetSMDiagnosticMessage" getSMDiagnosticMessage ::
  Ptr SMDiagnostic -> Ptr CUInt -> IO CString 

foreign import ccall unsafe "LLVM_Hs_GetSMDiagnosticLineContents" getSMDiagnosticLineContents ::
  Ptr SMDiagnostic -> Ptr CUInt -> IO CString 

