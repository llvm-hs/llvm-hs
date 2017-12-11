module LLVM.IRBuilder.Internal.SnocList where

import LLVM.Prelude

newtype SnocList a = SnocList { unSnocList :: [a] }
  deriving (Eq, Show)

instance Monoid (SnocList a) where
  mappend (SnocList xs) (SnocList ys) = SnocList $ ys ++ xs
  mempty = SnocList []

snoc :: SnocList a -> a -> SnocList a
snoc (SnocList xs) x = SnocList $ x : xs

getSnocList :: SnocList a -> [a]
getSnocList = reverse . unSnocList
