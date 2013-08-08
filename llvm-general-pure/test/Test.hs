import Test.Framework
import qualified LLVM.General.Test.Tests as LLVM.General

main = defaultMain [
        LLVM.General.tests
   ]
