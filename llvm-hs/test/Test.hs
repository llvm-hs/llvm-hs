import Test.Tasty
import qualified LLVM.Test.Tests as LLVM
import LLVM.CommandLine

main :: IO ()
main = defaultMain LLVM.tests
