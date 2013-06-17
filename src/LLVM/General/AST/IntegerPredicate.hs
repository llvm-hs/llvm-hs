{-# LANGUAGE
  DeriveDataTypeable 
  #-}  
module LLVM.General.AST.IntegerPredicate where

import Data.Data

data IntegerPredicate
  = EQ
  | NE
  | UGT
  | UGE
  | ULT
  | ULE
  | SGT
  | SGE
  | SLT
  | SLE
  deriving (Eq, Ord, Read, Show, Data, Typeable)



