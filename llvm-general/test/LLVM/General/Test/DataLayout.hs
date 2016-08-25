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

m s = "; ModuleID = '<string>'\nsource_filename = \"<string>\"\n" ++ s
t s = "target datalayout = \"" ++ s ++ "\"\n"
ddl = defaultDataLayout BigEndian

tests = testGroup "DataLayout" [
  testCase name $ strCheckC (Module "<string>" "<string>" mdl Nothing []) (m sdl) (m sdlc)
  | (name, mdl, sdl, sdlc) <- [
   ("none", Nothing, "", "")
  ] ++ [
   (name, Just mdl, t sdl, t (fromMaybe sdl msdlc))
   | (name, mdl, sdl, msdlc) <- [
    ("little-endian", defaultDataLayout LittleEndian, "e", Nothing),
    ("big-endian", defaultDataLayout BigEndian, "E", Nothing),
    ("native", ddl { nativeSizes = Just (Set.fromList [8,32]) }, "E-n8:32", Nothing),
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
     "E-p:8:64",
     Nothing
    ), (
     "no pref",
     ddl {
       pointerLayouts = 
         Map.insert (AddrSpace 1) (8, AlignmentInfo 32 (Just 64)) (pointerLayouts ddl)
     },
     "E-p1:8:32:64",
     Nothing
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
     "e-m:e-p:8:8:16-i1:8:256-i8:8:256-i16:16:256-i32:32:256-i64:64:256-v64:64:256-v128:128:256-f32:32:256-f64:64:256-f80:128:256-a:0:256-n8:16:32:64-S128",
     Nothing
    )
   ]
  ]
 ]
