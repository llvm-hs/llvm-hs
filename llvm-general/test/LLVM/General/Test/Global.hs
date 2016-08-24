module LLVM.General.Test.Global where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import LLVM.General.Test.Support

import LLVM.General.Context
import LLVM.General.Module
import LLVM.General.AST
import LLVM.General.AST.Type as A.T
import qualified LLVM.General.AST.Global as G

tests = testGroup "Global" [
  testGroup "Alignment" [
    testCase name $ withContext $ \context -> do
      let ast = Module "<string>" "<string>" Nothing Nothing [ GlobalDefinition g ]
      ast' <- withModuleFromAST' context ast moduleAST
      ast' @?= ast
    | a <- [0,1],
      s <- [Nothing, Just "foo"],
      g <- [
       globalVariableDefaults {
        G.name = UnName 0,
        G.type' = i32,
        G.alignment = a,
        G.section = s
        },
       functionDefaults {
         G.returnType = A.T.void,
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
