{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module LLVM.Test.Optimization where

import Test.Tasty
import Test.Tasty.HUnit

import LLVM.Test.Support

import Data.Functor
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Map as Map

import LLVM.Module
import LLVM.Context
import LLVM.Passes
import LLVM.Target

import LLVM.AST as A
import LLVM.AST.Type as A.T
import LLVM.AST.Name
import LLVM.AST.AddrSpace
import LLVM.AST.DataLayout
import qualified LLVM.AST.IntegerPredicate as IPred
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Visibility as V
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Constant as C

import qualified LLVM.Relocation as R
import qualified LLVM.CodeModel as CM
import qualified LLVM.CodeGenOpt as CGO

import Debug.Trace

handAST =
  Module "<string>" "<string>" Nothing Nothing [
      GlobalDefinition $ functionDefaults {
        G.returnType = i32,
        G.name = Name "foo",
        G.parameters = ([Parameter i32 (Name "x") []], False),
        G.functionAttributes = [Left (A.GroupID 0)],
        G.basicBlocks = [
          BasicBlock (UnName 0) [
           UnName 1 := Mul {
             nsw = False,
             nuw = False,
             operand0 = ConstantOperand (C.Int 32 6),
             operand1 = ConstantOperand (C.Int 32 7),
             metadata = []
           }
           ] (
             Do $ Br (Name "here") []
           ),
          BasicBlock (Name "here") [
           Name "go" := ICmp {
             iPredicate = IPred.EQ,
             operand0 = LocalReference i32 (UnName 1),
             operand1 = ConstantOperand (C.Int 32 42),
             metadata = []
           }
           ] (
              Do $ CondBr {
                condition = LocalReference i1 (Name "go"),
                trueDest = Name "take",
                falseDest = Name "done",
                metadata' = []
              }
           ),
          BasicBlock (Name "take") [
           UnName 2 := Sub {
             nsw = False,
             nuw = False,
             operand0 = LocalReference i32 (Name "x"),
             operand1 = LocalReference i32 (Name "x"),
             metadata = []
           }
           ] (
             Do $ Br (Name "done") []
           ),
          BasicBlock (Name "done") [
           Name "r" := Phi {
             type' = i32,
             incomingValues = [
               (LocalReference i32 (UnName 2), Name "take"),
               (ConstantOperand (C.Int 32 57), Name "here")
             ],
             metadata = []
           }
           ] (
             Do $ Ret (Just (LocalReference i32 (Name "r"))) []
           )
         ]
       },
      FunctionAttributes (A.GroupID 0) [A.NoUnwind, A.ReadNone, A.UWTable]
     ]

optimize :: PassSetSpec -> A.Module -> IO A.Module
optimize pss m = withContext $ \context -> withModuleFromAST context m $ \mIn' -> do
  runPasses pss mIn' 
  moduleAST mIn'

tests = testGroup "Optimization" [
  testCase "curated" $ do
    mOut <- optimize (PassSetSpec [CuratedPassSet 2] Nothing) handAST

    mOut @?= Module "<string>" "<string>" Nothing Nothing [
      GlobalDefinition $ functionDefaults {
        G.returnType = i32,
         G.name = Name "foo",
         G.parameters = ([Parameter i32 (Name "x") []], False),
         G.functionAttributes = [Left (A.GroupID 0)],
         G.basicBlocks = [
           BasicBlock (Name "here") [
              ] (
              Do $ Ret (Just (ConstantOperand (C.Int 32 0))) []
            )
          ]
        },
      FunctionAttributes (A.GroupID 0) [A.MustProgress, A.NoFree, A.NoRecurse, A.NoSync, A.NoUnwind, A.ReadNone, A.WillReturn, A.UWTable]
      ]
 ]
