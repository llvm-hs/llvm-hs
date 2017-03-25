{-# LANGUAGE OverloadedStrings #-}
module LLVM.Test.Global where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Monoid
import qualified Data.ByteString.Char8 as ByteString
import Data.ByteString.Short (fromShort)

import LLVM.Test.Support

import LLVM.Context
import LLVM.Module
import LLVM.AST
import LLVM.AST.Type as A.T
import qualified LLVM.AST.Global as G

tests = testGroup "Global" [
  testGroup "Alignment" [
    testCase name $ withContext $ \context -> do
      let ast = Module "<string>" "<string>" Nothing Nothing [ GlobalDefinition g ]
      ast' <- withModuleFromAST context ast moduleAST
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
          name = gn g <> ", align " <> show a <> (maybe "" (("  section " <>) . ByteString.unpack . fromShort) s)
   ]
 ]
