module LLVM.General.Test.Tests where

import Test.Framework

import qualified LLVM.General.Test.CallingConvention as CallingConvention
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
import qualified LLVM.General.Test.Analysis as Analysis
import qualified LLVM.General.Test.Linking as Linking
import qualified LLVM.General.Test.Instrumentation as Instrumentation
import qualified LLVM.General.Test.OrcJIT as OrcJIT

tests = testGroup "llvm-general" [
    CallingConvention.tests,
    Constants.tests,
    DataLayout.tests,
    ExecutionEngine.tests,
    Global.tests,
    InlineAssembly.tests,
    Instructions.tests,
    Metadata.tests,
    Module.tests,
    OrcJIT.tests,
    Optimization.tests,
    Target.tests,
    Analysis.tests,
    Linking.tests,
    Instrumentation.tests
  ]
