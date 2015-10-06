-- | Module to allow importing 'COMDAT.SelectionKind' distinctly qualified.
module LLVM.General.AST.COMDAT where

import LLVM.General.Prelude

-- | <http://llvm.org/docs/LangRef.html#comdats>
data SelectionKind
  = Any
  | ExactMatch
  | Largest
  | NoDuplicates
  | SameSize
  deriving (Eq, Read, Show, Typeable, Data)
