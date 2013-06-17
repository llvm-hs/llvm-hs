module LLVM.General.Test.Constants where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import LLVM.General.Test.Support

import Control.Monad
import Data.Functor
import Data.Maybe
import Foreign.Ptr
import Data.Word

import LLVM.General.Context
import LLVM.General.Module
import LLVM.General.Diagnostic
import LLVM.General.AST
import LLVM.General.AST.Type
import LLVM.General.AST.Name
import LLVM.General.AST.AddrSpace
import qualified LLVM.General.AST.Linkage as L
import qualified LLVM.General.AST.Visibility as V
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.IntegerPredicate as IPred

tests = testGroup "Constants" [
  testCase name $ strCheck mAST mStr
  | (name, type', value, str) <- [
    (
      "integer",
      IntegerType 32,
      C.Int (IntegerType 32) 1,
      "global i32 1"
    ), (
      "float",
      FloatingPointType 32,
      C.Float (FloatingPointType 32) 1,
      "global float 1.000000e+00"
    ), (
      "struct",
      StructureType False (replicate 2 (IntegerType 32)),
      C.Struct False (replicate 2 (C.Int (IntegerType 32) 1)),
      "global { i32, i32 } { i32 1, i32 1 }"
    ), (
      "dataarray",
      ArrayType 3 (IntegerType 32),
      C.Array (IntegerType 32) [C.Int (IntegerType 32) i | i <- [1,2,1]],
      "global [3 x i32] [i32 1, i32 2, i32 1]"
    ), (
      "array",
      ArrayType 3 (StructureType False [IntegerType 32]),
      C.Array (StructureType False [IntegerType 32]) [C.Struct False [C.Int (IntegerType 32) i] | i <- [1,2,1]],
      "global [3 x { i32 }] [{ i32 } { i32 1 }, { i32 } { i32 2 }, { i32 } { i32 1 }]"
    ), (
      "datavector",
      VectorType 3 (IntegerType 32),
      C.Vector [C.Int (IntegerType 32) i | i <- [1,2,1]],
      "global <3 x i32> <i32 1, i32 2, i32 1>"
    ), (
      "binop/cast",
      IntegerType 64,
      C.Add (C.PtrToInt (C.GlobalReference (UnName 1)) (IntegerType 64)) (C.Int (IntegerType 64) 2),
      "global i64 add (i64 ptrtoint (i32* @1 to i64), i64 2)"
    ), (
      "icmp",
      IntegerType 1,
      C.ICmp IPred.SGE (C.GlobalReference (UnName 1)) (C.GlobalReference (UnName 2)),
      "global i1 icmp sge (i32* @1, i32* @2)"
    ), (
      "getelementptr",
      PointerType (IntegerType 32) (AddrSpace 0),
      C.GetElementPtr True (C.GlobalReference (UnName 1)) [C.Int (IntegerType 64) 27],
      "global i32* getelementptr inbounds (i32* @1, i64 27)"
    ), (
      "selectvalue",
      IntegerType 32,
      C.Select (C.PtrToInt (C.GlobalReference (UnName 1)) (IntegerType 1)) 
         (C.Int (IntegerType 32) 1)
         (C.Int (IntegerType 32) 2),
      "global i32 select (i1 ptrtoint (i32* @1 to i1), i32 1, i32 2)"
    ), (
      "extractelement",
      IntegerType 32,
      C.ExtractElement
         (C.BitCast
             (C.PtrToInt (C.GlobalReference (UnName 1)) (IntegerType 64))
             (VectorType 2 (IntegerType 32)))
         (C.Int (IntegerType 32) 1),
      "global i32 extractelement (<2 x i32> bitcast (i64 ptrtoint (i32* @1 to i64) to <2 x i32>), i32 1)"
{-
    ), (
--  This test make llvm abort, as of llvm-3.2.
      "extractvalue",
      IntegerType 8,
      C.ExtractValue
        (C.Select (C.PtrToInt (C.GlobalReference (UnName 1)) (IntegerType 1)) 
         (C.Array (IntegerType 8) [C.Int (IntegerType 8) 0])
         (C.Array (IntegerType 8) [C.Int (IntegerType 8) 1]))
        [0],
      "global i8 extractvalue ([1 x i8] select (i1 ptrtoint (i32* @1 to i1), [1 x i8] [ i8 1 ], [1 x i8] [ i8 2 ]), 0)"
-}
    )
   ],
   let mAST = Module "<string>" Nothing Nothing [
             GlobalDefinition $ globalVariableDefaults {
               G.name = UnName 0, G.type' = type', G.initializer = Just value 
             },
             GlobalDefinition $ globalVariableDefaults {
               G.name = UnName 1, G.type' = IntegerType 32, G.initializer = Nothing 
             },
             GlobalDefinition $ globalVariableDefaults {
               G.name = UnName 2, G.type' = IntegerType 32, G.initializer = Nothing 
             }
           ]
       mStr = "; ModuleID = '<string>'\n\n@0 = " ++ str ++ "\n\
              \@1 = external global i32\n\
              \@2 = external global i32\n"
 ]
