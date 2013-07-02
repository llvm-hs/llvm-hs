module LLVM.General.Test.DataLayout where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import LLVM.General.Test.Support

import qualified Data.Set as Set
import qualified Data.Map as Map

import LLVM.General.Context
import LLVM.General.Module
import LLVM.General.AST
import LLVM.General.AST.DataLayout
import LLVM.General.AST.AddrSpace
import qualified LLVM.General.AST.Global as G

tests = testGroup "DataLayout" [
  testCase name $ strCheck (Module "<string>" mdl Nothing []) ("; ModuleID = '<string>'\n" ++ sdl)
  | (name, mdl, sdl) <- [
   ("none",Nothing, "")
  ] ++ [
   (name, Just mdl, "target datalayout = \"" ++ sdl ++ "\"\n")
   | (name, mdl, sdl) <- [
    ("little-endian", defaultDataLayout { endianness = Just LittleEndian }, "e"),
    ("big-endian", defaultDataLayout { endianness = Just BigEndian }, "E"),
    ("native", defaultDataLayout { nativeSizes = Just (Set.fromList [8,32]) }, "n8:32"),
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
     "p:8:64"
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
     "p1:8:32:64"
    )
   ]
  ]
 ]
