{-# LANGUAGE CPP #-}
module LLVM.IRBuilder.Internal.SnocList where

import LLVM.Prelude

newtype SnocList a = SnocList { unSnocList :: [a] }
  deriving (Eq, Show)

instance Semigroup (SnocList a) where
  SnocList xs <> SnocList ys = SnocList $ ys ++ xs

instance Monoid (SnocList a) where
#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
#endif
  mempty = SnocList []

snoc :: SnocList a -> a -> SnocList a
snoc (SnocList xs) x = SnocList $ x : xs

getSnocList :: SnocList a -> [a]
getSnocList = reverse . unSnocList
