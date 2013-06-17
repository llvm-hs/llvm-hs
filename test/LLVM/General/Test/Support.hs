module LLVM.General.Test.Support where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.Functor

import LLVM.General.Context
import LLVM.General.Module
import LLVM.General.Diagnostic

withModuleFromString' c s f  = do
  e <- withModuleFromString c s f
  case e of
    Right r -> return r
    Left d -> do
           let e = diagnosticDisplay d
           putStrLn e
           fail e

strCheck mAST mStr = withContext $ \context -> do
  a <- withModuleFromString' context mStr moduleAST
  s <- either error id <$> withModuleFromAST context mAST moduleString
  (a,s) @?= (mAST, mStr)

  
