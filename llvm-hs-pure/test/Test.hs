import Test.Framework
import qualified LLVM.Test.Tests as LLVM

main = defaultMain [
        LLVM.tests
   ]
