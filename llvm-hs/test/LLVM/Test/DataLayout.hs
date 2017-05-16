{-# LANGUAGE OverloadedStrings #-}
module LLVM.Test.DataLayout where

import Test.Tasty
import Test.Tasty.HUnit

import LLVM.Test.Support

import Control.Monad.IO.Class
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Map as Map

import LLVM.Context
import LLVM.Module
import LLVM.AST
import LLVM.AST.DataLayout
import LLVM.AST.AddrSpace
import qualified LLVM.AST.Global as G
import LLVM.Internal.Coding
import LLVM.Internal.DataLayout
import LLVM.Internal.EncodeAST
import LLVM.Internal.FFI.DataLayout (getTypeAllocSize)

m s = "; ModuleID = '<string>'\nsource_filename = \"<string>\"\n" <> s
t s = "target datalayout = \"" <> s <> "\"\n"
ddl = defaultDataLayout BigEndian

tests = testGroup "DataLayout" $
  testCase "getTypeAllocSize" (withContext $ \ctx -> runEncodeAST ctx $ do
    ty <- encodeM (IntegerType 8)
    liftIO $ do
      size <-
        withFFIDataLayout
          (ddl { typeLayouts = Map.singleton (IntegerAlign, 8) (AlignmentInfo 8 8) })
          (\dl -> getTypeAllocSize dl ty)
      size @?= 1
      size <-
        withFFIDataLayout
          (ddl { typeLayouts = Map.singleton (IntegerAlign, 8) (AlignmentInfo 32 32) })
          (\dl -> getTypeAllocSize dl ty)
      size @?= 4)
  :
  [
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
          AlignmentInfo 64 64
         )
     },
     "E-p:8:64",
     Nothing
    ), (
     "no pref",
     ddl {
       pointerLayouts = 
         Map.insert (AddrSpace 1) (8, AlignmentInfo 32 64) (pointerLayouts ddl)
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
     "e-m:e-p:8:8:16-i1:8:256-i8:8:256-i16:16:256-i32:32:256-i64:64:256-v64:64:256-v128:128:256-f32:32:256-f64:64:256-f80:128:256-a:0:256-n8:16:32:64-S128",
     Nothing
    )
   ]
  ]
 ]
