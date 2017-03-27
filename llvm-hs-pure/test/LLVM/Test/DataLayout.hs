{-# LANGUAGE OverloadedStrings #-}
module LLVM.Test.DataLayout where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Control.Monad.Trans.Except

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Map as Map

import LLVM.AST
import LLVM.AST.DataLayout
import LLVM.AST.AddrSpace
import LLVM.DataLayout

ddl = defaultDataLayout LittleEndian

mergeWithDefaultDL :: DataLayout -> DataLayout
mergeWithDefaultDL dl =
  dl
  { pointerLayouts = pointerLayouts dl <> pointerLayouts (defaultDataLayout LittleEndian)
  , typeLayouts = typeLayouts dl <> typeLayouts (defaultDataLayout LittleEndian)
  }

instance Arbitrary Endianness where
  arbitrary = elements [LittleEndian, BigEndian]

instance Arbitrary AddrSpace where
  arbitrary = AddrSpace <$> arbitrary

instance Arbitrary Mangling where
  arbitrary =
    elements [ELFMangling, MIPSMangling, MachOMangling, WindowsCOFFMangling]

instance Arbitrary AlignmentInfo where
  arbitrary = AlignmentInfo <$> arbitrary <*> arbitrary

instance Arbitrary AlignType where
  arbitrary = elements [IntegerAlign, VectorAlign, FloatAlign]

instance Arbitrary DataLayout where
  arbitrary =
    DataLayout
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

tests = testGroup "DataLayout" $
  testProperty "roundtrip"
    (\dl -> pure (Just (mergeWithDefaultDL dl)) == parseDataLayout LittleEndian (dataLayoutToString dl))
  :
  [
  testCase name $ do
    let Right parsed = runExcept $ parseDataLayout LittleEndian strDl
    (dataLayoutToString astDl, parsed) @?= (strDl, Just astDl)
  | (name, astDl, strDl) <- [
    ("little-endian", ddl, "e"),
    ("big-endian", defaultDataLayout BigEndian, "E"),
    ("native", ddl { nativeSizes = Just (Set.fromList [8,32]) }, "e-n8:32"),
    (
     "no pref",
     ddl {
       pointerLayouts = 
         Map.singleton
         (AddrSpace 0) 
         (
          8,
          AlignmentInfo 64 64
         )
     },
     "e-p:8:64"
    ), (
     "pref",
     ddl {
       pointerLayouts = 
         Map.insert (AddrSpace 1) (8, AlignmentInfo 32 64) (pointerLayouts ddl)
     },
     "e-p1:8:32:64"
    ), (
     "def",
     ddl { pointerLayouts = Map.singleton (AddrSpace 0) (64, AlignmentInfo 64 64) },
     "e"
    ), (
     "big",
     ddl {
       endianness = LittleEndian,
       mangling = Just ELFMangling,
       stackAlignment = Just 128,
       pointerLayouts = Map.fromList [
         (AddrSpace 0, (8, AlignmentInfo 8 16))
        ],
       typeLayouts = Map.fromList [
         ((IntegerAlign, 1), AlignmentInfo 8 256),
         ((IntegerAlign, 8), AlignmentInfo 8 256),
         ((IntegerAlign, 16), AlignmentInfo 16 256),
         ((IntegerAlign, 32), AlignmentInfo 32 256),
         ((IntegerAlign, 64), AlignmentInfo 64 256),
         ((VectorAlign, 64), AlignmentInfo 64 256),
         ((VectorAlign, 128), AlignmentInfo 128 256),
         ((FloatAlign, 32), AlignmentInfo 32 256),
         ((FloatAlign, 64), AlignmentInfo 64 256),
         ((FloatAlign, 80), AlignmentInfo 128 256)
        ] `Map.union` typeLayouts ddl, 
       aggregateLayout = AlignmentInfo 0 256,
       nativeSizes = Just (Set.fromList [8,16,32,64])
     },
     "e-m:e-p:8:8:16-i1:8:256-i8:8:256-i16:16:256-i32:32:256-i64:64:256-v64:64:256-v128:128:256-f32:32:256-f64:64:256-f80:128:256-a:0:256-n8:16:32:64-S128"
    )
   ]
 ]
