-- | Diagnostics describe parse errors
module LLVM.General.Diagnostic (
  DiagnosticKind(..),
  Diagnostic(..),
  diagnosticDisplay
 ) where

import LLVM.General.Prelude

-- | What kind of problem does a diagnostic describe?
data DiagnosticKind 
  = ErrorKind
  | WarningKind
  | NoteKind
  deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | A 'Diagnostic' described a problem during parsing of LLVM IR
data Diagnostic = Diagnostic {
    lineNumber :: Int,
    columnNumber :: Int,
    diagnosticKind :: DiagnosticKind,
    filename :: String,
    message :: String,
    lineContents :: String
  }
  deriving (Eq, Ord, Read, Show)

-- | Convert a 'Diagnostic' to a printable form.
diagnosticDisplay :: Diagnostic -> String
diagnosticDisplay d = 
  (filename d) ++ ":" ++ show (lineNumber d) ++ ":" ++ show (columnNumber d)
  ++ ":\n" ++ show (diagnosticKind d) ++ ": " ++ (message d) ++ "\n"
  ++ (lineContents d) ++ "\n"
