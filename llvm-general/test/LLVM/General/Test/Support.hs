module LLVM.General.Test.Support where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.Functor
import Control.Monad
import Control.Monad.Trans.Except

import LLVM.General.Context
import LLVM.General.Module
import LLVM.General.Diagnostic
import LLVM.General.PrettyPrint

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

assertEqPretty :: (Eq a, PrettyShow a) => a -> a -> Assertion
assertEqPretty actual expected = do
  let showPretty = showPrettyEx 80 "  " shortPrefixScheme
  assertBool 
   ("expected: " ++ showPretty expected ++ "\n" ++ "but got: " ++ showPretty actual ++ "\n")
   (expected == actual)

strCheckC mAST mStr mStrCanon = withContext $ \context -> do
  a <- withModuleFromLLVMAssembly' context mStr moduleAST
  s <- withModuleFromAST' context mAST moduleLLVMAssembly
  (a,s) `assertEqPretty` (mAST, mStrCanon)

strCheck mAST mStr = strCheckC mAST mStr mStr

