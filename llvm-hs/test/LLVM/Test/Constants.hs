{-# LANGUAGE OverloadedStrings #-}
module LLVM.Test.Constants where

import Test.Tasty
import Test.Tasty.HUnit

import LLVM.Test.Support

import Control.Monad
import Data.Functor
import Data.Maybe
import Data.Monoid
import Foreign.Ptr
import Data.Word

import LLVM.Context
import LLVM.Module
import LLVM.Diagnostic
import LLVM.AST
import LLVM.AST.Type
import LLVM.AST.Name
import LLVM.AST.AddrSpace
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Visibility as V
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.IntegerPredicate as IPred

tests = testGroup "Constants" [
  testCase name $ strCheck mAST mStr
  | (name, type', value, str) <- [
    (
      "integer",
      i32,
      C.Int 32 1,
      "global i32 1"
    ), (
      "wide integer",
      IntegerType 65,
      C.Int 65 1,
      "global i65 1"
    ), (
      "big wide integer",
      IntegerType 66,
      C.Int 66 20000000000000000000,
      "global i66 20000000000000000000"
    ), (
      "negative wide integer",
      IntegerType 65,
      C.Int 65 36893488147419103231,
      "global i65 -1"
    ), (
      "half",
      half,
      C.Float (F.Half 0x1234),
      "global half 0xH1234"
    ), (
      "float",
      float,
      C.Float (F.Single 1),
      "global float 1.000000e+00"
    ), (
      "double",
      double,
      C.Float (F.Double 1),
      "global double 1.000000e+00"
    ), (
      "quad",
      fp128,
      C.Float (F.Quadruple 0x0007000600050004 0x0003000200010000),
      "global fp128 0xL00030002000100000007000600050004" -- yes, this order is weird
    ), (
      "quad 1.0",
      fp128,
      C.Float (F.Quadruple 0x3fff000000000000 0x0000000000000000),
      "global fp128 0xL00000000000000003FFF000000000000" -- yes, this order is weird
    ), (
      "x86_fp80",
      x86_fp80,
      C.Float (F.X86_FP80 0x0004 0x8003000200010000),
      "global x86_fp80 0xK00048003000200010000"
{- don't know how to test this - LLVM's handling of this weird type is even weirder
    ), (
      "ppc_fp128",
      ppc_fp128,
      C.Float (F.PPC_FP128 0x0007000600050004 0x0003000200010000),
      "global ppc_fp128 0xM????????????????"
-}
    ), (
      "struct",
      StructureType False (replicate 2 i32),
      C.Struct Nothing False (replicate 2 (C.Int 32 1)),
      "global { i32, i32 } { i32 1, i32 1 }"
    ), (
      "dataarray",
      ArrayType 3 i32,
      C.Array i32 [C.Int 32 i | i <- [1,2,1]],
      "global [3 x i32] [i32 1, i32 2, i32 1]"
    ), (
      "array",
      ArrayType 3 (StructureType False [i32]),
      C.Array (StructureType False [i32]) [C.Struct Nothing False [C.Int 32 i] | i <- [1,2,1]],
      "global [3 x { i32 }] [{ i32 } { i32 1 }, { i32 } { i32 2 }, { i32 } { i32 1 }]"
    ), (
      "datavector",
      VectorType 3 i32,
      C.Vector [C.Int 32 i | i <- [1,2,1]],
      "global <3 x i32> <i32 1, i32 2, i32 1>"
    ), (
      "undef",
      i32,
      C.Undef i32,
      "global i32 undef"
    ), (
      "binop/cast",
      i64,
      C.Add False False (C.PtrToInt (C.GlobalReference (ptr i32) (UnName 1)) i64) (C.Int 64 2),
      "global i64 add (i64 ptrtoint (i32* @1 to i64), i64 2)"
    ), (
      "binop/cast nsw",
      i64,
      C.Add True False (C.PtrToInt (C.GlobalReference (ptr i32) (UnName 1)) i64) (C.Int 64 2),
      "global i64 add nsw (i64 ptrtoint (i32* @1 to i64), i64 2)"
    ), (
      "icmp",
      i1,
      C.ICmp IPred.SGE (C.GlobalReference (ptr i32) (UnName 1)) (C.GlobalReference (ptr i32) (UnName 2)),
      "global i1 icmp sge (i32* @1, i32* @2)"
    ), (
      "getelementptr",
      ptr i32,
      C.GetElementPtr True (C.GlobalReference (ptr i32) (UnName 1)) [C.Int 64 27],
      "global i32* getelementptr inbounds (i32, i32* @1, i64 27)"
    ), (
      "selectvalue",
      i32,
      C.Select (C.PtrToInt (C.GlobalReference (ptr i32) (UnName 1)) i1)
         (C.Int 32 1)
         (C.Int 32 2),
      "global i32 select (i1 ptrtoint (i32* @1 to i1), i32 1, i32 2)"
    ), (
      "extractelement",
      i32,
      C.ExtractElement
         (C.BitCast
             (C.PtrToInt (C.GlobalReference (ptr i32) (UnName 1)) i64)
             (VectorType 2 i32))
         (C.Int 32 1),
      "global i32 extractelement (<2 x i32> bitcast (i64 ptrtoint (i32* @1 to i64) to <2 x i32>), i32 1)"
    ), (
     "addrspacecast",
     (PointerType i32 (AddrSpace 1)),
     C.AddrSpaceCast (C.GlobalReference (ptr i32) (UnName 1)) (PointerType i32 (AddrSpace 1)),
     "global i32 addrspace(1)* addrspacecast (i32* @1 to i32 addrspace(1)*)"
{-    ), (
-- This test fails since LLVM 3.2!
-- LLVM parses the extractValue instruction from a file via llvm-as properly, but it does not here.
-- Rewritten by Andrew April 2021 to match the specific example in the LLVM language reference:
-- https://llvm.org/docs/LangRef.html#extractvalue-instruction
-- Bug filed with upstream here: https://bugs.llvm.org/show_bug.cgi?id=50092
      "extractvalue",
      i32,
      C.ExtractValue
        (C.Struct Nothing False [C.Int 32 0, C.Int 32 1])
        [0],
      "global i32 extractvalue ({i32, i32} {i32 0, i32 1}, 0)"
-}
    )
   ],
   let mAST = Module "<string>" "<string>" Nothing Nothing [
             GlobalDefinition $ globalVariableDefaults {
               G.name = UnName 0, G.type' = type', G.initializer = Just value
             },
             GlobalDefinition $ globalVariableDefaults {
               G.name = UnName 1, G.type' = i32, G.initializer = Nothing
             },
             GlobalDefinition $ globalVariableDefaults {
               G.name = UnName 2, G.type' = i32, G.initializer = Nothing
             }
           ]
       mStr = "; ModuleID = '<string>'\nsource_filename = \"<string>\"\n\n@0 = " <> str <> "\n\
              \@1 = external global i32\n\
              \@2 = external global i32\n"
 ]
