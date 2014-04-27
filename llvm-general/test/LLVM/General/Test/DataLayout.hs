module LLVM.General.Test.DataLayout where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import LLVM.General.Test.Support

import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map

import LLVM.General.Context
import LLVM.General.Module
import LLVM.General.AST
import LLVM.General.AST.DataLayout
import LLVM.General.AST.AddrSpace
import qualified LLVM.General.AST.Global as G

m s = "; ModuleID = '<string>'\n" ++ s
t s = "target datalayout = \"" ++ s ++ "\"\n"

tests = testGroup "DataLayout" [
  testCase name $ strCheckC (Module "<string>" mdl Nothing []) (m sdl) (m sdlc)
  | (name, mdl, sdl, sdlc) <- [
   ("none", Nothing, "", "")
  ] ++ [
   (name, Just mdl, t sdl, t (fromMaybe sdl msdlc))
   | (name, mdl, sdl, msdlc) <- [
    ("little-endian", defaultDataLayout { endianness = Just LittleEndian }, "e", Nothing),
    ("big-endian", defaultDataLayout { endianness = Just BigEndian }, "E", Nothing),
    ("native", defaultDataLayout { nativeSizes = Just (Set.fromList [8,32]) }, "n8:32", Nothing),
    (
     "no pref",
     defaultDataLayout {
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
     "p:8:64",
     Nothing
    ), (
     "no pref",
     defaultDataLayout {
       pointerLayouts = 
         Map.singleton
         (AddrSpace 1) 
         (
          8,
          AlignmentInfo {
            abiAlignment = 32,
            preferredAlignment = Just 64
          }
         )
     },
     "p1:8:32:64",
     Nothing
    ), (
     "big",
     DataLayout {
       endianness = Just LittleEndian,
       stackAlignment = Just 128,
       mangling = Just ELFMangling,
       pointerLayouts = Map.fromList [
         (AddrSpace 0, (64, AlignmentInfo {abiAlignment = 64, preferredAlignment = Just 64}))
        ],
       typeLayouts = Map.fromList [
         ((IntegerAlign, 1), AlignmentInfo {abiAlignment = 8, preferredAlignment = Just 8}),
         ((IntegerAlign, 8), AlignmentInfo {abiAlignment = 8, preferredAlignment = Just 8}),
         ((IntegerAlign, 16), AlignmentInfo {abiAlignment = 16, preferredAlignment = Just 16}),
         ((IntegerAlign, 32), AlignmentInfo {abiAlignment = 32, preferredAlignment = Just 32}),
         ((IntegerAlign, 64), AlignmentInfo {abiAlignment = 64, preferredAlignment = Just 64}),
         ((VectorAlign, 64), AlignmentInfo {abiAlignment = 64, preferredAlignment = Just 64}),
         ((VectorAlign, 128), AlignmentInfo {abiAlignment = 128, preferredAlignment = Just 128}),
         ((FloatAlign, 32), AlignmentInfo {abiAlignment = 32, preferredAlignment = Just 32}),
         ((FloatAlign, 64), AlignmentInfo {abiAlignment = 64, preferredAlignment = Just 64}),
         ((FloatAlign, 80), AlignmentInfo {abiAlignment = 128, preferredAlignment = Just 128}),
         ((AggregateAlign, 0), AlignmentInfo {abiAlignment = 0, preferredAlignment = Just 64})
        ],
       nativeSizes = Just (Set.fromList [8,16,32,64])
     },
     "e-m:e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-f80:128:128-n8:16:32:64-S128",
     Just "e-m:e-S128-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-v64:64:64-v128:128:128-f32:32:32-f64:64:64-f80:128:128-a0:0:64-n8:16:32:64"
    )
   ]
  ]
 ]
