-- | This module is presents a prelude mostly like the post-Applicative-Monad world of
-- base >= 4.8 / ghc >= 7.10, as well as the post-Semigroup-Monoid world of
-- base >= 4.11 / ghc >= 8.4, even on earlier versions. It's intended as an internal library
-- for llvm-hs-pure and llvm-hs; it's exposed only to be shared between the two.
module LLVM.Prelude (
    module Prelude,
    module Data.Data,
    module GHC.Generics,
    module Data.Int,
    module Data.Word,
    module Data.Functor,
    module Data.Foldable,
    module Data.Semigroup,
    module Data.Traversable,
    module Control.Applicative,
    module Control.Monad,
    ByteString,
    ShortByteString,
    fromMaybe,
    leftBiasedZip,
    findM,
    ifM
    ) where

import Prelude hiding (
    mapM, mapM_,
    sequence, sequence_,
    concat,
    foldr, foldr1, foldl, foldl1,
    minimum, maximum, sum, product, all, any, and, or,
    concatMap,
    elem, notElem,
  )
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)
import Data.Int
import Data.Maybe (fromMaybe)
import Data.Word
import Data.Functor
import Data.Foldable
import Data.Semigroup (Semigroup((<>)))
import Data.Traversable
import Control.Applicative
import Control.Monad hiding (
    forM, forM_,
    mapM, mapM_,
    sequence, sequence_,
    msum
  )

import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)

leftBiasedZip :: [a] -> [b] -> [(a, Maybe b)]
leftBiasedZip [] _ = []
leftBiasedZip xs [] = map (, Nothing) xs
leftBiasedZip (x:xs) (y:ys) = (x, Just y) : leftBiasedZip xs ys

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond ifTrue ifFalse = do
  cond' <- cond
  if cond'
    then ifTrue
    else ifFalse

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM p (x:xs) = ifM (p x) (return $ Just x) (findM p xs)
