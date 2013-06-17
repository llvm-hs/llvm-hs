{-# LANGUAGE
  DeriveDataTypeable 
  #-}  
module LLVM.General.AST.RMWOperation where

import Data.Data

data RMWOperation
  = Xchg
  | Add
  | Sub
  | And
  | Nand
  | Or
  | Xor
  | Max
  | Min
  | UMax
  | UMin
  deriving (Eq, Ord, Read, Show, Data, Typeable)



