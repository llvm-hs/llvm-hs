{-# LANGUAGE
  TemplateHaskell,
  MultiParamTypeClasses
  #-}  
module LLVM.Internal.Diagnostic where

import LLVM.Prelude

import qualified LLVM.Internal.FFI.LLVMCTypes as FFI
import qualified LLVM.Internal.FFI.SMDiagnostic as FFI

import Control.Exception

import Foreign.Ptr

import LLVM.Diagnostic
import LLVM.Internal.Coding
import LLVM.Internal.String ()

genCodingInstance [t| DiagnosticKind |] ''FFI.DiagnosticKind [
    (FFI.diagnosticKindError, ErrorKind),
    (FFI.diagnosticKindWarning, WarningKind),
    (FFI.diagnosticKindNote, NoteKind)
  ]

withSMDiagnostic :: (Ptr FFI.SMDiagnostic -> IO a) -> IO a
withSMDiagnostic = bracket FFI.createSMDiagnostic FFI.disposeSMDiagnostic

getDiagnostic :: Ptr FFI.SMDiagnostic -> IO Diagnostic
getDiagnostic p = do
  l <- decodeM =<< FFI.getSMDiagnosticLineNo p
  c <- decodeM =<< FFI.getSMDiagnosticColumnNo p
  k <- decodeM =<< FFI.getSMDiagnosticKind p
  f <- decodeM $ FFI.getSMDiagnosticFilename p
  m <- decodeM $ FFI.getSMDiagnosticMessage p
  lc <- decodeM $ FFI.getSMDiagnosticLineContents p
  return $ Diagnostic { 
    lineNumber = l, columnNumber = c, diagnosticKind = k, filename = f, message = m, lineContents = lc
   }

