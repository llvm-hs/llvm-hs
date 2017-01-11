module LLVM.Test.Tests where

import Test.Framework

import qualified LLVM.Test.DataLayout as DataLayout
import qualified LLVM.Test.PrettyPrint as PrettyPrint

tests = testGroup "llvm-hs" [
    DataLayout.tests,
    PrettyPrint.tests
  ]
