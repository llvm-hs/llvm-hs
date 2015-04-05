-- | Module to allow importing 'Linkage' distinctly qualified.
module LLVM.General.AST.Linkage where

import LLVM.General.Prelude

-- | <http://llvm.org/docs/LangRef.html#linkage>
data Linkage
    = Private
    | LinkerPrivate
    | LinkerPrivateWeak
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
    | DLLImport
    | DLLExport
  deriving (Eq, Read, Show, Typeable, Data)
