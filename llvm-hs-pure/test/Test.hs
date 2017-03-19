import Test.Tasty as X
import qualified LLVM.Test.Tests as LLVM

main = defaultMain LLVM.tests
