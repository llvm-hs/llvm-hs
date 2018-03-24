module LLVM.Test.Tests where

import Test.Tasty

import qualified LLVM.Test.DataLayout as DataLayout
import qualified LLVM.Test.IRBuilder as IRBuilder

tests = testGroup "llvm-hs"
  [ DataLayout.tests
  , IRBuilder.tests
  ]
