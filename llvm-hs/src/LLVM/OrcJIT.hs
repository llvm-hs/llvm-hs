module LLVM.OrcJIT (
    -- * CompileLayer
    CompileLayer,
    -- ** Add/remove modules
    ModuleSetHandle,
    addModuleSet,
    removeModuleSet,
    withModuleSet,
    -- ** Search for symbols
    findSymbol,
    findSymbolIn,
    JITSymbol(..),
    JITSymbolFlags(..),
    -- ** Symbol mangling
    MangledSymbol,
    mangleSymbol,
    -- ** Create compile layers
    IRCompileLayer,
    withIRCompileLayer,
    CompileOnDemandLayer,
    withCompileOnDemandLayer,
    IRTransformLayer,
    withIRTransformLayer,
    -- ** Dispose compile layers
    disposeCompileLayer,
    -- * LinkingLayer
    LinkingLayer,
    ObjectLinkingLayer,
    withObjectLinkingLayer,
    -- * Misc
    SymbolResolver(..),
    JITCompileCallbackManager,
    withJITCompileCallbackManager,
    IndirectStubsManagerBuilder,
    withIndirectStubsManagerBuilder,
  ) where

import LLVM.Internal.OrcJIT
import LLVM.Internal.OrcJIT.CompileLayer
import LLVM.Internal.OrcJIT.CompileOnDemandLayer
import LLVM.Internal.OrcJIT.IRCompileLayer
import LLVM.Internal.OrcJIT.IRTransformLayer
