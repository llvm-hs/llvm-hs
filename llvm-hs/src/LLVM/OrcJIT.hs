module LLVM.OrcJIT (
    -- * CompileLayer
    CompileLayer,
    -- ** Add/remove modules
    ModuleHandle,
    addModule,
    removeModule,
    withModule,
    -- ** Search for symbols
    JITSymbol(..),
    JITSymbolError(..),
    JITSymbolFlags(..),
    defaultJITSymbolFlags,
    SymbolResolver(..),
    -- ** Symbol mangling
    MangledSymbol,
    mangleSymbol,
    -- ** IRCompileLayer
    IRCompileLayer,
    newIRCompileLayer,
    withIRCompileLayer,
    -- ** CompileOnDemandLayer
    CompileOnDemandLayer,
    newCompileOnDemandLayer,
    withCompileOnDemandLayer,
    -- ** IRTRansformLayer
    IRTransformLayer,
    newIRTransformLayer,
    withIRTransformLayer,
    -- ** Dispose of compile layers
    disposeCompileLayer,
    -- * LinkingLayer
    LinkingLayer,
    -- ** Create linking layers
    ObjectLinkingLayer,
    newObjectLinkingLayer,
    withObjectLinkingLayer,
    -- ** Dispose of linking layers
    disposeLinkingLayer,
    -- ** Add an object file
    addObjectFile,
    -- * JITCompileCallbackManager
    JITCompileCallbackManager,
    newJITCompileCallbackManager,
    disposeJITCompileCallbackManager,
    withJITCompileCallbackManager,
    -- * IndirectStubsManagerBuilder
    IndirectStubsManagerBuilder,
    newIndirectStubsManagerBuilder,
    disposeIndirectStubsManagerBuilder,
    withIndirectStubsManagerBuilder,
  ) where

import LLVM.Internal.OrcJIT
import LLVM.Internal.OrcJIT.CompileLayer
import LLVM.Internal.OrcJIT.LinkingLayer
import LLVM.Internal.OrcJIT.CompileOnDemandLayer
import LLVM.Internal.OrcJIT.IRCompileLayer
import LLVM.Internal.OrcJIT.IRTransformLayer
