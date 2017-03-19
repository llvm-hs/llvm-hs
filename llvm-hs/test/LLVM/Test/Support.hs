module LLVM.Test.Support where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Functor
import Control.Monad
import Control.Monad.Trans.Except
import Text.Show.Pretty

import LLVM.Context
import LLVM.Module
import LLVM.Diagnostic

class FailInIO f where
  errorToString :: f -> String

failInIO :: FailInIO f => ExceptT f IO a -> IO a
failInIO = either (fail . errorToString) return <=< runExceptT

instance FailInIO String where
  errorToString = id

instance FailInIO (Either String Diagnostic) where
  errorToString = either id diagnosticDisplay

withModuleFromLLVMAssembly' c s f  = failInIO $ withModuleFromLLVMAssembly c s f
withModuleFromAST' c a f = failInIO $ withModuleFromAST c a f
withModuleFromBitcode' c a f = failInIO $ withModuleFromBitcode c ("<string>", a) f

assertEqPretty :: (Eq a, Show a) => a -> a -> Assertion
assertEqPretty actual expected = do
  assertBool
   ("expected: " ++ ppShow expected ++ "\n" ++ "but got: " ++ ppShow actual ++ "\n")
   (expected == actual)

strCheckC mAST mStr mStrCanon = withContext $ \context -> do
  a <- withModuleFromLLVMAssembly' context mStr moduleAST
  s <- withModuleFromAST' context mAST moduleLLVMAssembly
  (a,s) `assertEqPretty` (mAST, mStrCanon)

strCheck mAST mStr = strCheckC mAST mStr mStr
