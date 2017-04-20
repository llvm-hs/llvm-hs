{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import qualified LLVM.Test.Tests as LLVM
import LLVM.CommandLine

main = do
  parseCommandLineOptions [
    "test",
    "-bb-vectorize-ignore-target-info"
   ] Nothing
  defaultMain LLVM.tests
