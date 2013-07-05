module LLVM.General.Test.Support where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.Functor
import Control.Monad
import Control.Monad.Error

import LLVM.General.Context
import LLVM.General.Module
import LLVM.General.Diagnostic

class FailInIO f where
  errorToString :: f -> String

failInIO :: FailInIO f => ErrorT f IO a -> IO a
failInIO = either (fail . errorToString) return <=< runErrorT

instance FailInIO String where
  errorToString = id

instance FailInIO (Either String Diagnostic) where
  errorToString = either id diagnosticDisplay

withModuleFromString' c s f  = failInIO $ withModuleFromString c s f
withModuleFromAST' c a f = failInIO $ withModuleFromAST c a f

strCheck mAST mStr = withContext $ \context -> do
  a <- withModuleFromString' context mStr moduleAST
  s <- withModuleFromAST' context mAST moduleString
  (a,s) @?= (mAST, mStr)

