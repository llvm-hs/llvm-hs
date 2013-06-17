{-# LANGUAGE
  DeriveDataTypeable
  #-}
module LLVM.General.Diagnostic (
  DiagnosticKind(..),
  Diagnostic(..),
  diagnosticDisplay
 ) where

import Data.Data

data DiagnosticKind 
  = ErrorKind
  | WarningKind
  | NoteKind
  deriving (Eq, Ord, Read, Show, Typeable, Data)

data Diagnostic = Diagnostic {
    lineNumber :: Int,
    columnNumber :: Int,
    diagnosticKind :: DiagnosticKind,
    filename :: String,
    message :: String,
    lineContents :: String
  }
  deriving (Eq, Ord, Read, Show)

diagnosticDisplay :: Diagnostic -> String
diagnosticDisplay d = 
  (filename d) ++ ":" ++ show (lineNumber d) ++ ":" ++ show (columnNumber d)
  ++ ":\n" ++ show (diagnosticKind d) ++ ": " ++ (message d) ++ "\n"
  ++ (lineContents d) ++ "\n"
