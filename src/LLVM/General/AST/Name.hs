module LLVM.General.AST.Name where

import Data.Word

data Name 
    = Name String
    | UnName Word
   deriving (Eq, Ord, Read, Show)

