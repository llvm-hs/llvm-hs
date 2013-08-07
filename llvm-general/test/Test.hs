import Test.Framework
import qualified LLVM.General.Test.Tests as LLVM.General
import LLVM.General.CommandLine

main = do
  parseCommandLineOptions [
    "test",
    "-bb-vectorize-ignore-target-info"
   ] Nothing
  defaultMain [
        LLVM.General.tests
   ]
