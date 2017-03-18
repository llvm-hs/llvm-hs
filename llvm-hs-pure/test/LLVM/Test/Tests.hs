module LLVM.Test.Tests where

import Test.Framework

import qualified LLVM.Test.DataLayout as DataLayout

tests = testGroup "llvm-hs" [
    DataLayout.tests
  ]
