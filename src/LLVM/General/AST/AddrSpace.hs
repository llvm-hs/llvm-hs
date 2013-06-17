module LLVM.General.AST.AddrSpace where

import Data.Word

data AddrSpace = AddrSpace Word32
   deriving (Eq, Ord, Read, Show)
