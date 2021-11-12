import Test.Tasty
import qualified LLVM.Test.Tests as LLVM
import LLVM.CommandLine
import LLVM.Target (initializeAllTargets)

main :: IO ()
main = do
  initializeAllTargets
  defaultMain LLVM.tests
