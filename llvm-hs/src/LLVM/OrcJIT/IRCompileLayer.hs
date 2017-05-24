module LLVM.OrcJIT.IRCompileLayer (
    IRCompileLayer,
    ModuleSetHandle,
    findSymbol,
    findSymbolIn,
    mangleSymbol,
    withIRCompileLayer,
    withModuleSet
  ) where

import LLVM.Internal.OrcJIT.CompileLayer
import LLVM.Internal.OrcJIT.IRCompileLayer
import LLVM.Internal.FFI.OrcJIT.CompileLayer (ModuleSetHandle)
