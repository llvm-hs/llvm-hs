module LLVM.Test.Tests where

import Test.Tasty

import qualified LLVM.Test.Analysis as Analysis
import qualified LLVM.Test.Attribute as Attribute
import qualified LLVM.Test.CallingConvention as CallingConvention
import qualified LLVM.Test.Constants as Constants
import qualified LLVM.Test.DataLayout as DataLayout
import qualified LLVM.Test.ExecutionEngine as ExecutionEngine
import qualified LLVM.Test.FunctionAttribute as FunctionAttribute
import qualified LLVM.Test.Global as Global
import qualified LLVM.Test.InlineAssembly as InlineAssembly
import qualified LLVM.Test.Instructions as Instructions
import qualified LLVM.Test.Instrumentation as Instrumentation
import qualified LLVM.Test.Linking as Linking
import qualified LLVM.Test.Metadata as Metadata
import qualified LLVM.Test.Module as Module
import qualified LLVM.Test.ObjectCode as ObjectCode
import qualified LLVM.Test.Optimization as Optimization
import qualified LLVM.Test.OrcJIT as OrcJIT
import qualified LLVM.Test.ParameterAttribute as ParameterAttribute
import qualified LLVM.Test.Target as Target
import qualified LLVM.Test.Regression as Regression
import qualified LLVM.Test.Support as Support


tests = Support.withTargets $ testGroup "llvm-hs" [
    CallingConvention.tests,
    Constants.tests,
    DataLayout.tests,
    Attribute.tests,
    FunctionAttribute.tests,
    ExecutionEngine.tests,
    Global.tests,
    InlineAssembly.tests,
    Instructions.tests,
    Metadata.tests,
    Module.tests,
    OrcJIT.tests,
    Optimization.tests,
    ParameterAttribute.tests,
    Target.tests,
    Analysis.tests,
    Linking.tests,
    Instrumentation.tests,
    ObjectCode.tests,
    Regression.tests
  ]
