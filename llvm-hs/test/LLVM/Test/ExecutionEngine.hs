{-# LANGUAGE
  FlexibleContexts,
  ForeignFunctionInterface,
  OverloadedStrings
  #-}
module LLVM.Test.ExecutionEngine where

import Test.Tasty
import Test.Tasty.HUnit

import LLVM.Test.Support

import Control.Monad
import Data.Functor
import Data.Maybe

import Foreign.Ptr
import Data.Word

import LLVM.Context
import LLVM.Module
import LLVM.ExecutionEngine
import LLVM.AST
import LLVM.AST.Type
import LLVM.AST.Name
import LLVM.AST.AddrSpace
import qualified LLVM.AST.IntegerPredicate as IPred

import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Visibility as V
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Constant as C

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

  s <- withModuleFromAST context mAST $ \m -> do
        withModuleInEngine executionEngine m $ \em -> do
          Just p <- getFunction em (Name "_foo")
          (mkIO32Stub ((castFunPtr p) :: FunPtr (Word32 -> IO Word32))) 7
  s @?= 42

tests = testGroup "ExecutionEngine" [
  testCase "run something with MCJIT" $ testJIT (\c -> withMCJIT c Nothing Nothing Nothing Nothing)
 ]
