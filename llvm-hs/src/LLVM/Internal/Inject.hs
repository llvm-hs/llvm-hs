{-# LANGUAGE MultiParamTypeClasses #-}
module LLVM.Internal.Inject where

import LLVM.Prelude

class Inject a b where
  inject :: a -> b

instance Inject a a where
  inject = id

