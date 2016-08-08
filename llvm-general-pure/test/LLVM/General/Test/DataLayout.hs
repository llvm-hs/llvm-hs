module LLVM.General.Test.DataLayout where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Control.Monad.Trans.Except

import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map

import LLVM.General.AST
import LLVM.General.AST.DataLayout
import LLVM.General.AST.AddrSpace
import LLVM.General.DataLayout

ddl = defaultDataLayout LittleEndian

tests = testGroup "DataLayout" [
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
          AlignmentInfo {
            abiAlignment = 64,
            preferredAlignment = Nothing
          }
         )
     },
     "e-p:8:64"
    ), (
     "pref",
     ddl {
       pointerLayouts = 
         Map.insert (AddrSpace 1) (8, AlignmentInfo 32 (Just 64)) (pointerLayouts ddl)
     },
     "e-p1:8:32:64"
    ), (
     "def",
     ddl { pointerLayouts = Map.singleton (AddrSpace 0) (64, AlignmentInfo 64 (Just 64)) },
     "e"
    ), (
     "big",
     ddl {
       endianness = LittleEndian,
       mangling = Just ELFMangling,
       stackAlignment = Just 128,
       pointerLayouts = Map.fromList [
         (AddrSpace 0, (8, AlignmentInfo {abiAlignment = 8, preferredAlignment = Just 16}))
        ],
       typeLayouts = Map.fromList [
         ((IntegerAlign, 1), AlignmentInfo {abiAlignment = 8, preferredAlignment = Just 256}),
         ((IntegerAlign, 8), AlignmentInfo {abiAlignment = 8, preferredAlignment = Just 256}),
         ((IntegerAlign, 16), AlignmentInfo {abiAlignment = 16, preferredAlignment = Just 256}),
         ((IntegerAlign, 32), AlignmentInfo {abiAlignment = 32, preferredAlignment = Just 256}),
         ((IntegerAlign, 64), AlignmentInfo {abiAlignment = 64, preferredAlignment = Just 256}),
         ((VectorAlign, 64), AlignmentInfo {abiAlignment = 64, preferredAlignment = Just 256}),
         ((VectorAlign, 128), AlignmentInfo {abiAlignment = 128, preferredAlignment = Just 256}),
         ((FloatAlign, 32), AlignmentInfo {abiAlignment = 32, preferredAlignment = Just 256}),
         ((FloatAlign, 64), AlignmentInfo {abiAlignment = 64, preferredAlignment = Just 256}),
         ((FloatAlign, 80), AlignmentInfo {abiAlignment = 128, preferredAlignment = Just 256})
        ] `Map.union` typeLayouts ddl, 
       aggregateLayout = AlignmentInfo {abiAlignment = 0, preferredAlignment = Just 256},
       nativeSizes = Just (Set.fromList [8,16,32,64])
     },
     "e-m:e-p:8:8:16-i1:8:256-i8:8:256-i16:16:256-i32:32:256-i64:64:256-v64:64:256-v128:128:256-f32:32:256-f64:64:256-f80:128:256-a:0:256-n8:16:32:64-S128"
    )
   ]
 ]
