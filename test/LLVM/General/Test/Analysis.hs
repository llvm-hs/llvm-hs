module LLVM.General.Test.Analysis where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import LLVM.General.Test.Support

import Control.Monad.Error

import LLVM.General.Module
import LLVM.General.Context
import LLVM.General.Analysis

import LLVM.General.AST as A
import LLVM.General.AST.Type
import LLVM.General.AST.Name
import LLVM.General.AST.AddrSpace
import LLVM.General.AST.DataLayout
import qualified LLVM.General.AST.IntegerPredicate as IPred
import qualified LLVM.General.AST.Linkage as L
import qualified LLVM.General.AST.Visibility as V
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Constant as C

import qualified LLVM.General.Relocation as R
import qualified LLVM.General.CodeModel as CM
import qualified LLVM.General.CodeGenOpt as CGO

tests = testGroup "Analysis" [
  testGroup "Verifier" [
    testCase "Module" $ withContext $ \context -> do
      let ast = Module "<string>" Nothing Nothing [
            GlobalDefinition $ Function L.External V.Default CC.C [] VoidType (Name "foo") ([
                Parameter (IntegerType 32) (Name "x") []
               ],False)
             [] 
             Nothing 0         
             [
              BasicBlock (UnName 0) [
                UnName 1 := Call {
                  isTailCall = False,
                  callingConvention = CC.C,
                  returnAttributes = [],
                  function = Right (ConstantOperand (C.GlobalReference (Name "foo"))),
                  arguments = [
                   (ConstantOperand (C.Int 8 1), [])
                  ],
                  functionAttributes = [],
                  metadata = []
                 }
              ] (
                Do $ Ret Nothing []
              )
             ]
            ]
      Left s <- withModuleFromAST' context ast $ runErrorT . verify
      s @?= "Call parameter type does not match function signature!\n\
            \i8 1\n\
            \ i32  call void @foo(i8 1)\n\
            \Broken module found, compilation terminated.\n\
            \Broken module found, compilation terminated.\n"
      | False -- this test will cause an assertion if LLVM is compiled with assertions on.
   ]
 ]
