{-# LANGUAGE
  DeriveDataTypeable
  #-}

module LLVM.General.Relocation where

import Data.Data

data Model 
    = Default
    | Static
    | PIC
    | DynamicNoPIC
    deriving (Eq, Read, Show, Typeable, Data)
