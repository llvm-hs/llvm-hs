module LLVM.General.Test.Tests where

import Test.Framework

import qualified LLVM.General.Test.Constants as Constants
import qualified LLVM.General.Test.DataLayout as DataLayout
import qualified LLVM.General.Test.ExecutionEngine as ExecutionEngine
import qualified LLVM.General.Test.Global as Global
import qualified LLVM.General.Test.InlineAssembly as InlineAssembly
import qualified LLVM.General.Test.Instructions as Instructions
import qualified LLVM.General.Test.Metadata as Metadata
import qualified LLVM.General.Test.Module as Module
import qualified LLVM.General.Test.Optimization as Optimization
import qualified LLVM.General.Test.Target as Target

tests = testGroup "llvm-general" [
    Constants.tests,
    DataLayout.tests,
    ExecutionEngine.tests,
    Global.tests,
    InlineAssembly.tests,
    Instructions.tests,
    Metadata.tests,
    Module.tests,
    Optimization.tests,
    Target.tests
  ]
