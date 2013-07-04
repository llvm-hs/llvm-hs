module LLVM.General.Test.Global where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import LLVM.General.Test.Support

import LLVM.General.Context
import LLVM.General.Module
import LLVM.General.AST
import qualified LLVM.General.AST.Global as G

tests = testGroup "Global" [
  testGroup "Alignment" [
    testCase name $ withContext $ \context -> do
      let ast = Module "<string>" Nothing Nothing [ GlobalDefinition g ]
      ast' <- withModuleFromAST' context ast moduleAST
      ast' @?= ast
    | a <- [0,1],
      s <- [Nothing, Just "foo"],
      g <- [
       globalVariableDefaults {
        G.name = UnName 0,
        G.type' = IntegerType 32,
        G.alignment = a,
        G.section = s
        },
       functionDefaults {
         G.returnType = VoidType,
         G.name = UnName 0,
         G.parameters = ([], False),
         G.alignment = a,
         G.section = s
       }
       ],
      let
          gn (G.Function {}) = "function"
          gn (G.GlobalVariable {}) = "variable"
          name = gn g ++ ", align " ++ show a ++ (maybe "" ("  section " ++ ) s)
   ]
 ]
