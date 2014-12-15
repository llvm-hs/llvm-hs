module LLVM.General.Test.Linking where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import LLVM.General.Test.Support

import Control.Monad.Trans.Except
import Data.Functor
import qualified Data.Set as Set
import qualified Data.Map as Map

import LLVM.General.Module
import LLVM.General.Context
import LLVM.General.PassManager
import LLVM.General.Transforms
import LLVM.General.Target

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

tests = testGroup "Linking" [
  testCase "basic" $ do
    let 
      ast0 = Module "<string>" Nothing Nothing [
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
      ast1 = Module "<string>" Nothing Nothing [
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
      withModuleFromAST' context ast0 $ \m0 ->
        withModuleFromAST' context ast1 $ \m1 -> do
          runExceptT $ linkModules False m0 m1
          moduleAST m0
    [ n | GlobalDefinition g <- defs, let Name n = G.name g ] @?= [ "private0", "external0", "external1" ]
 ]
