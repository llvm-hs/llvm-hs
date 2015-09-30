-- | Module to allow importing 'ThreadLocalStorage.Model' distinctly qualified.
module LLVM.General.AST.ThreadLocalStorage where

import LLVM.General.Prelude

-- | <http://llvm.org/docs/LangRef.html#thread-local-storage-models>
data Model
    = GeneralDynamic
    | LocalDynamic
    | InitialExec
    | LocalExec
  deriving (Eq, Ord, Read, Show, Typeable, Data)
