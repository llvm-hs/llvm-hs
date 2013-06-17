{-# LANGUAGE
  DeriveDataTypeable
  #-}
module LLVM.General.AST.InlineAssembly where

import Data.Data

import LLVM.General.AST.Type

data Dialect
  = ATTDialect
  | IntelDialect
  deriving (Eq, Read, Show, Typeable, Data)

data InlineAssembly
  = InlineAssembly {
      type' :: Type,
      assembly :: String,
      constraints :: String,
      hasSideEffects :: Bool,
      alignStack :: Bool,
      dialect :: Dialect
    }
  deriving (Eq, Read, Show)
