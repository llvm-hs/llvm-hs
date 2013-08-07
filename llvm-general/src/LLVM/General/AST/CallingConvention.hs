-- | Module to allow importing 'CallingConvention' distinctly qualified.
module LLVM.General.AST.CallingConvention where

import Data.Data
import Data.Word

-- |  <http://llvm.org/docs/LangRef.html#callingconv>
data CallingConvention = C | Fast | Cold | GHC | Numbered Word32
  deriving (Eq, Read, Show, Typeable, Data)

