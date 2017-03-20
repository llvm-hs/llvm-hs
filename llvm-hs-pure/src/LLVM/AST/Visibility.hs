-- | Module to allow importing 'Visibility' distinctly qualified.
module LLVM.AST.Visibility where

import LLVM.Prelude

-- | <http://llvm.org/docs/LangRef.html#visibility>
data Visibility = Default | Hidden | Protected
  deriving (Eq, Read, Show, Typeable, Data, Generic)
