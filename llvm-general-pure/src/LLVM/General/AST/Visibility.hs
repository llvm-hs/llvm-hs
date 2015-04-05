-- | Module to allow importing 'Visibility' distinctly qualified.
module LLVM.General.AST.Visibility where

import LLVM.General.Prelude

-- | <http://llvm.org/docs/LangRef.html#visibility>
data Visibility = Default | Hidden | Protected
  deriving (Eq, Read, Show, Typeable, Data)
