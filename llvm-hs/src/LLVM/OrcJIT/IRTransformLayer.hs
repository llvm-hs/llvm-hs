module LLVM.OrcJIT.IRTransformLayer (
    IRTransformLayer,
    ModuleSetHandle,
    findSymbol,
    mangleSymbol,
    withIRTransformLayer,
    withModuleSet
  ) where

import LLVM.Internal.OrcJIT.CompileLayer
import LLVM.Internal.OrcJIT.IRTransformLayer
import LLVM.Internal.FFI.OrcJIT.CompileLayer (ModuleSetHandle)
