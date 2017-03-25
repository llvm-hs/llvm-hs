{-# LANGUAGE OverloadedStrings #-}
module LLVM.Test.Linking where

import Test.Tasty
import Test.Tasty.HUnit

import LLVM.Test.Support

import Control.Monad.Trans.Except
import Data.Functor
import qualified Data.Set as Set
import qualified Data.Map as Map

import LLVM.Module
import LLVM.Context
import LLVM.PassManager
import LLVM.Transforms
import LLVM.Target

import LLVM.AST as A
import LLVM.AST.Type
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

tests = testGroup "Linking" [
  testCase "basic" $ do
    let 
      ast0 = Module "<string>" "<string>" Nothing Nothing [
          GlobalDefinition $ functionDefaults {
             G.linkage = L.Private,
             G.returnType = i32,
             G.name = Name "private0"
           },
          GlobalDefinition $ functionDefaults {
             G.linkage = L.External,
             G.returnType = i32,
             G.name = Name "external0"
           }
        ]
      ast1 = Module "<string>" "<string>" Nothing Nothing [
          GlobalDefinition $ functionDefaults {
             G.linkage = L.Private,
             G.returnType = i32,
             G.name = Name "private1"
           },
          GlobalDefinition $ functionDefaults {
             G.linkage = L.External,
             G.returnType = i32,
             G.name = Name "external1"
           }
        ]      

    Module { moduleDefinitions = defs } <- withContext $ \context -> 
      withModuleFromAST context ast0 $ \dest -> do
      withModuleFromAST context ast0 $ \src -> do
        linkModules dest src
        moduleAST dest
    [ n | GlobalDefinition g <- defs, let Name n = G.name g ] @?= [ "private0", "external0" ]
 ]
