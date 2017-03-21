-- | Module to allow importing 'Linkage' distinctly qualified.
module LLVM.AST.Linkage where

import LLVM.Prelude

-- | <http://llvm.org/docs/LangRef.html#linkage>
data Linkage
    = Private
    | Internal
    | AvailableExternally
    | LinkOnce
    | Weak
    | Common
    | Appending
    | ExternWeak
    | LinkOnceODR
    | WeakODR
    | External
  deriving (Eq, Read, Show, Typeable, Data, Generic)
