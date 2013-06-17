{-# LANGUAGE
  DeriveDataTypeable 
  #-}  
module LLVM.General.AST.FloatingPointPredicate where

import Data.Data

data FloatingPointPredicate
  = False
  | OEQ
  | OGT
  | OGE
  | OLT
  | OLE
  | ONE
  | ORD
  | UNO
  | UEQ
  | UGT
  | UGE
  | ULT
  | ULE
  | UNE
  | True
  deriving (Eq, Ord, Read, Show, Data, Typeable)



