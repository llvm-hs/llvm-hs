-- | Module to allow importing 'Linkage' distinctly qualified.
module LLVM.General.AST.Linkage where

import LLVM.General.Prelude

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
  deriving (Eq, Read, Show, Typeable, Data)
