{-# LANGUAGE MultiParamTypeClasses #-}
module LLVM.General.Internal.Inject where

import LLVM.General.Prelude

class Inject a b where
  inject :: a -> b

instance Inject a a where
  inject = id

