module LLVM.Test.Tests where

import Test.Tasty

import qualified LLVM.Test.DataLayout as DataLayout

tests = testGroup "llvm-hs" [
    DataLayout.tests
  ]
