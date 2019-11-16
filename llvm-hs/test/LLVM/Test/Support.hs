{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module LLVM.Test.Support where

import LLVM.Prelude

import Test.Tasty
import Test.Tasty.HUnit
import Test.QuickCheck

import Data.ByteString (ByteString)
import qualified Data.ByteString.Short as BSS
import Data.Functor
import Control.Monad
import Control.Monad.Trans.Except
import Text.Show.Pretty

import LLVM.Context
import LLVM.Module
import LLVM.Diagnostic

withModuleFromLLVMAssembly' :: Context -> ByteString -> (Module -> IO a) -> IO a
withModuleFromLLVMAssembly' c s f  = withModuleFromLLVMAssembly c s f
withModuleFromBitcode' c a f = withModuleFromBitcode c ("<string>", a) f

assertEqPretty :: (Eq a, Show a) => a -> a -> Assertion
assertEqPretty actual expected = do
  assertBool
   ("expected: " ++ ppShow expected ++ "\n" ++ "but got: " ++ ppShow actual ++ "\n")
   (expected == actual)

strCheckC mAST mStr mStrCanon = withContext $ \context -> do
  a <- withModuleFromLLVMAssembly' context mStr moduleAST
  s <- withModuleFromAST context mAST moduleLLVMAssembly
  (a,s) `assertEqPretty` (mAST, mStrCanon)

strCheck mAST mStr = strCheckC mAST mStr mStr

arbitrarySbs :: Gen ShortByteString
arbitrarySbs = BSS.pack <$> listOf arbitrary
