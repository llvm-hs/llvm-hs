{-# LANGUAGE OverloadedStrings #-}
module LLVM.Test.InlineAssembly where

import Test.Tasty
import Test.Tasty.HUnit

import LLVM.Test.Support

import LLVM.Context
import LLVM.Module

import LLVM.AST
import LLVM.AST.Type
import LLVM.AST.InlineAssembly as IA
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Visibility as V
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Global as G

tests = testGroup "InlineAssembly" [
  testCase "expression" $ do
    let ast = Module "<string>" "<string>" Nothing Nothing [
                GlobalDefinition $ 
                  functionDefaults {
                    G.returnType = i32,
                    G.name = Name "foo",
                    G.parameters = ([Parameter i32 (Name "x") []],False),
                    G.basicBlocks = [
                      BasicBlock (UnName 0) [
                        UnName 1 := Call {
                          tailCallKind = Nothing,
                          callingConvention = CC.C,
                          returnAttributes = [],
                          function = Left $ InlineAssembly {
                                       IA.type' = FunctionType i32 [i32] False,
                                       assembly = "bswap $0",
                                       constraints = "=r,r",
                                       hasSideEffects = False,
                                       alignStack = False,
                                       dialect = ATTDialect
                                     },
                          arguments = [
                            (LocalReference i32 (Name "x"), [])
                           ],
                          functionAttributes = [],
                          metadata = []
                        }
                      ] (
                        Do $ Ret (Just (LocalReference i32 (UnName 1))) []
                      )
                    ]
                }

              ]
        s = "; ModuleID = '<string>'\n\
             \source_filename = \"<string>\"\n\
             \\n\
             \define i32 @foo(i32 %x) {\n\
             \  %1 = call i32 asm \"bswap $0\", \"=r,r\"(i32 %x)\n\
             \  ret i32 %1\n\
             \}\n"
    strCheck ast s,

  testCase "module" $ do
    let ast = Module "<string>" "<string>" Nothing Nothing [
                ModuleInlineAssembly "foo",
                ModuleInlineAssembly "bar",
                GlobalDefinition $ globalVariableDefaults {
                  G.name = UnName 0,
                  G.type' = i32
                }
              ]
        s = "; ModuleID = '<string>'\n\
             \source_filename = \"<string>\"\n\
             \\n\
             \module asm \"foo\"\n\
             \module asm \"bar\"\n\
             \\n\
             \@0 = external global i32\n"
    strCheck ast s
 ]
