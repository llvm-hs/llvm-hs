module LLVM.Test.PrettyPrint where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import LLVM.PrettyPrint

import LLVM.AST
import LLVM.AST.Type
import LLVM.AST.Global
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Visibility as V
import qualified LLVM.AST.DLL as DLL
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C

tests = testGroup "PrettyPrint" [
  testCase "basic" $ do
    let ast = Module "<string>" "<string>" Nothing Nothing [
          GlobalDefinition $ functionDefaults {
            dllStorageClass = Just DLL.Export,
            returnType = i32,
            name = Name "foo",
            parameters = ([Parameter i32 (Name "x") []], False),
            basicBlocks = [
              BasicBlock (UnName 0) [
                UnName 1 := Mul {
                  nsw = True,
                  nuw = False,
                  operand0 = ConstantOperand (C.Int 32 1),
                  operand1 = ConstantOperand (C.Int 32 1),
                  metadata = []
                }
               ] (
                 Do $ Br (Name "here") []
               ),
              BasicBlock (Name "here") [
               ] (
                 Do $ Ret (Just (LocalReference i32 (UnName 1))) []
               )
             ]
           }
         ]
        s = "A.Module {\n\
            \  A.moduleName = \"<string>\",\n\
            \  A.moduleSourceFileName = \"<string>\",\n\
            \  A.moduleDataLayout = Nothing,\n\
            \  A.moduleTargetTriple = Nothing,\n\
            \  A.moduleDefinitions = [\n\
            \    A.GlobalDefinition A.G.Function {\n\
            \      A.G.linkage = A.L.External,\n\
            \      A.G.visibility = A.V.Default,\n\
            \      A.G.dllStorageClass = Just A.DLL.Export,\n\
            \      A.G.callingConvention = A.CC.C,\n\
            \      A.G.returnAttributes = [],\n\
            \      A.G.returnType = A.IntegerType {A.typeBits = 32},\n\
            \      A.G.name = A.Name \"foo\",\n\
            \      A.G.parameters = ([A.G.Parameter A.IntegerType { A.typeBits = 32 } (A.Name \"x\") []], False),\n\
            \      A.G.functionAttributes = [],\n\
            \      A.G.section = Nothing,\n\
            \      A.G.comdat = Nothing,\n\
            \      A.G.alignment = 0,\n\
            \      A.G.garbageCollectorName = Nothing,\n\
            \      A.G.prefix = Nothing,\n\
            \      A.G.basicBlocks = [\n\
            \        A.G.BasicBlock (A.UnName 0) [\n\
            \          A.UnName 1 A.:= A.Mul {\n\
            \            A.nsw = True,\n\
            \            A.nuw = False,\n\
            \            A.operand0 = A.ConstantOperand A.C.Int {A.C.integerBits = 32, A.C.integerValue = 1},\n\
            \            A.operand1 = A.ConstantOperand A.C.Int {A.C.integerBits = 32, A.C.integerValue = 1},\n\
            \            A.metadata = []\n\
            \          }\n\
            \        ] (A.Do A.Br { A.dest = A.Name \"here\", A.metadata' = [] }),\n\
            \        A.G.BasicBlock (A.Name \"here\") [] (\n\
            \          A.Do A.Ret {\n\
            \            A.returnOperand = Just (A.LocalReference A.IntegerType { A.typeBits = 32 } (A.UnName 1)),\n\
            \            A.metadata' = []\n\
            \          }\n\
            \        )\n\
            \      ],\n\
            \      A.G.personalityFunction = Nothing\n\
            \    }\n\
            \  ]\n\
            \}"
    showPretty ast @?= s,
  testCase "imports" $ do
    imports defaultPrefixScheme @?=
      "import Data.Either\n\
      \import qualified Data.Map as Map\n\
      \import Data.Maybe\n\
      \import qualified Data.Set as Set\n\
      \import qualified LLVM.AST as A\n\
      \import qualified LLVM.AST.AddrSpace as A\n\
      \import qualified LLVM.AST.Attribute as A.A\n\
      \import qualified LLVM.AST.COMDAT as A.COMDAT\n\
      \import qualified LLVM.AST.CallingConvention as A.CC\n\
      \import qualified LLVM.AST.Constant as A.C\n\
      \import qualified LLVM.AST.DLL as A.DLL\n\
      \import qualified LLVM.AST.DataLayout as A\n\
      \import qualified LLVM.AST.Float as A\n\
      \import qualified LLVM.AST.FloatingPointPredicate as A.FPred\n\
      \import qualified LLVM.AST.Global as A.G\n\
      \import qualified LLVM.AST.InlineAssembly as A\n\
      \import qualified LLVM.AST.Instruction as A\n\
      \import qualified LLVM.AST.IntegerPredicate as A.IPred\n\
      \import qualified LLVM.AST.Linkage as A.L\n\
      \import qualified LLVM.AST.Name as A\n\
      \import qualified LLVM.AST.Operand as A\n\
      \import qualified LLVM.AST.Type as A\n\
      \import qualified LLVM.AST.Visibility as A.V\n"
 ]

