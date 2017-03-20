-- | Module to allow importing 'COMDAT.SelectionKind' distinctly qualified.
module LLVM.AST.COMDAT where

import LLVM.Prelude

-- | <http://llvm.org/docs/LangRef.html#comdats>
data SelectionKind
  = Any
  | ExactMatch
  | Largest
  | NoDuplicates
  | SameSize
  deriving (Eq, Read, Show, Typeable, Data, Generic)
