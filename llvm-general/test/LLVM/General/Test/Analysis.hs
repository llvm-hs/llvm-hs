module LLVM.General.Test.Analysis where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import LLVM.General.Test.Support

import Control.Monad.Trans.Except

import LLVM.General.Module
import LLVM.General.Context
import LLVM.General.Analysis

import LLVM.General.AST as A
import LLVM.General.AST.Type as A.T
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
{-
    -- this test will cause an assertion if LLVM is compiled with assertions on.
    testCase "Module" $ do
      let ast = Module "<string>" Nothing Nothing [
            GlobalDefinition $ Function L.External V.Default CC.C [] A.T.void (Name "foo") ([
                Parameter i32 (Name "x") []
               ],False)
             [] 
             Nothing 0 Nothing         
             [
              BasicBlock (UnName 0) [
                UnName 1 := Call {
                  isTailCall = False,
                  callingConvention = CC.C,
                  returnAttributes = [],
                  function = Right (ConstantOperand (C.GlobalReference (A.T.FunctionType A.T.void [A.T.i32] False) (Name "foo"))),
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
      Left s <- withContext $ \context -> withModuleFromAST' context ast $ runExceptT . verify
      s @?= "Call parameter type does not match function signature!\n\
            \i8 1\n\
            \ i32  call void @foo(i8 1)\n\
            \Broken module found, compilation terminated.\n\
            \Broken module found, compilation terminated.\n",
-}

    testGroup "regression" [
      testCase "load synchronization" $ do
       let str = "; ModuleID = '<string>'\n\
                 \\n\
                 \define double @my_function2(double* %input_0) {\n\
                 \foo:\n\
                 \  %tmp_input_w0 = getelementptr inbounds double* %input_0, i64 0\n\
                 \  %0 = load double* %tmp_input_w0, align 8\n\
                 \  ret double %0\n\
                 \}\n"
           ast = 
             Module "<string>" Nothing Nothing [
               GlobalDefinition $ functionDefaults {
                 G.returnType = double,
                 G.name = Name "my_function2",
                 G.parameters = ([
                   Parameter (ptr double) (Name "input_0") []
                  ],False),
                 G.basicBlocks = [
                   BasicBlock (Name "foo") [ 
                    Name "tmp_input_w0" := GetElementPtr {
                      inBounds = True,
                      address = LocalReference (ptr double) (Name "input_0"),
                      indices = [ConstantOperand (C.Int 64 0)],
                      metadata = []
                    },
                    UnName 0 := Load {
                      volatile = False,
                      address = LocalReference (ptr double) (Name "tmp_input_w0"),
                      maybeAtomicity = Nothing,
                      alignment = 8,
                      metadata = []
                    }
                   ] (
                     Do $ Ret (Just (LocalReference double (UnName 0))) []
                   )
                  ]
                }
              ]
       strCheck ast str
       s <- withContext $ \context -> withModuleFromAST' context ast $ runExceptT . verify
       s @?= Right ()
     ]
   ]
 ]
