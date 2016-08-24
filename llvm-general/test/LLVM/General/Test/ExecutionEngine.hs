{-# LANGUAGE
  ForeignFunctionInterface
  #-}
module LLVM.General.Test.ExecutionEngine where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import LLVM.General.Test.Support

import Control.Monad
import Data.Functor
import Data.Maybe

import Foreign.Ptr
import Data.Word

import LLVM.General.Context
import LLVM.General.Module
import LLVM.General.ExecutionEngine
import LLVM.General.AST
import LLVM.General.AST.Type
import LLVM.General.AST.Name
import LLVM.General.AST.AddrSpace
import qualified LLVM.General.AST.IntegerPredicate as IPred

import qualified LLVM.General.AST.Linkage as L
import qualified LLVM.General.AST.Visibility as V
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Constant as C

foreign import ccall "dynamic" mkIO32Stub :: FunPtr (Word32 -> IO Word32) -> (Word32 -> IO Word32)

testJIT :: ExecutionEngine e (FunPtr ()) => (Context -> (e -> IO ()) -> IO ()) -> Assertion
testJIT withEE = withContext $ \context -> withEE context $ \executionEngine -> do
  let mAST = Module "runSomethingModule" "runSomethingModule" Nothing Nothing [
              GlobalDefinition $ functionDefaults {
                G.returnType = i32,
                G.name = Name "_foo",
                G.parameters = ([Parameter i32 (Name "bar") []],False),
                G.basicBlocks = [
                  BasicBlock (UnName 0) [] (
                    Do $ Ret (Just (ConstantOperand (C.Int 32 42))) []
                  )
                ]
               }
              ]

  s <- withModuleFromAST' context mAST $ \m -> do
        withModuleInEngine executionEngine m $ \em -> do
          Just p <- getFunction em (Name "_foo")
          (mkIO32Stub ((castFunPtr p) :: FunPtr (Word32 -> IO Word32))) 7
  s @?= 42

tests = testGroup "ExecutionEngine" [
  testCase "run something with MCJIT" $ testJIT (\c -> withMCJIT c Nothing Nothing Nothing Nothing)
 ]
