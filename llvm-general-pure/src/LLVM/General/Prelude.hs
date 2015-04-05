{-# LANGUAGE CPP #-}
module LLVM.General.Prelude (
    module Prelude,
    module Data.Data,
    module Data.Int,
    module Data.Word,
    module Control.Monad
    ) where

import Prelude
import Data.Data hiding (typeOf)
import Data.Int
import Data.Word
import Control.Monad
