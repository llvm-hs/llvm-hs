{-# LANGUAGE
  DeriveDataTypeable
  #-}

module LLVM.General.CodeModel where

import Data.Data

data Model
    = Default
    | JITDefault
    | Small
    | Kernel
    | Medium
    | Large
    deriving (Eq, Read, Show, Typeable, Data)
