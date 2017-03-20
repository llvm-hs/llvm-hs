-- | Module to allow importing 'DLL.StorageClass' distinctly qualified.
module LLVM.AST.DLL where

import LLVM.Prelude

-- | <http://llvm.org/docs/LangRef.html#dll-storage-classes>
data StorageClass
    = Import
    | Export
  deriving (Eq, Read, Show, Typeable, Data, Generic)
