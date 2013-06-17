{-# LANGUAGE
  DeriveDataTypeable
  #-}

module LLVM.General.CodeGenOpt where

import Data.Data

data Level
    = None
    | Less
    | Default
    | Aggressive
    deriving (Eq, Ord, Read, Show, Typeable, Data)