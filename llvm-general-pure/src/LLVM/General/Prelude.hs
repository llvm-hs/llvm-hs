{-# LANGUAGE CPP #-}
module LLVM.General.Prelude (
    module Prelude,
    module Data.Data,
    module Data.Int,
    module Data.Word,
    module Data.Functor,
    module Data.Foldable,
    module Data.Traversable,
    module Control.Applicative,
    module Control.Monad
    ) where

import Prelude hiding (
    mapM, mapM_,
    sequence, sequence_,
    concat,
    foldr, foldr1, foldl, foldl1,
    minimum, maximum, sum, product, all, any, and, or,
    concatMap,
    elem, notElem,
    msum,
  )
import Data.Data hiding (typeOf)
import Data.Int
import Data.Word
import Data.Functor
import Data.Foldable
import Data.Traversable
import Control.Applicative
import Control.Monad hiding (
    forM, forM_,
    mapM, mapM_,
    sequence, sequence_,
    msum
  )
