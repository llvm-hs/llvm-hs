module LLVM.General.Test.Tests where

import Test.Framework

import qualified LLVM.General.Test.PrettyPrint as PrettyPrint

tests = testGroup "llvm-general" [
    PrettyPrint.tests
  ]
