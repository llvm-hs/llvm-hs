{-# LANGUAGE ForeignFunctionInterface #-}
module LLVM.General.Test.ExecutionEngine where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

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

tests = testGroup "ExecutionEngine" [

  testCase "runSomething" $ withContext $ \context -> withExecutionEngine context $ \executionEngine -> do
    let mAST = Module "runSomethingModule" Nothing Nothing [
                GlobalDefinition $ Function L.External V.Default CC.C [] (IntegerType 32) (Name "foo") ([
                              Parameter (IntegerType 32) (Name "foo") []
                             ],False) [] 
                 Nothing 0
                 [
                  BasicBlock (UnName 0) [] (
                    Do $ Ret (Just (ConstantOperand (C.Int 32 42))) []
                   )
                 ]
                ]
    
    s <- withModuleFromAST context mAST $ \m -> do
          withModuleInEngine executionEngine m $ do
            Just p <- findFunction executionEngine (Name "foo")
            (mkIO32Stub ((castPtrToFunPtr p) :: FunPtr (Word32 -> IO Word32))) 7
    s @?= Right 42
 ]
