module LLVM.OrcJIT (
    -- * CompileLayer
    CompileLayer,
    -- ** Add/remove modules
    ModuleKey,
    -- TODO(llvm-12): Remove unused APIs.
    -- LLVM.Internal.OrcJIT.CompileLayer.addModule,
    -- removeModule,
    -- withModule,
    -- ** Search for symbols
    JITSymbol(..),
    JITSymbolError(..),
    JITSymbolFlags(..),
    defaultJITSymbolFlags,
    SymbolResolver(..),
    withSymbolResolver,
    -- ** Symbol mangling
    MangledSymbol(..),
    -- TODO: Remove prefix.
    LLVM.Internal.OrcJITV2.mangleSymbol,
    -- ** ExecutionSession
    ExecutionSession,
    createExecutionSession,
    disposeExecutionSession,
    withExecutionSession,
    -- allocateModuleKey,
    -- releaseModuleKey,
    -- withModuleKey,
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
    -- * OrcJITV2
    JITDylib(..),
    lookupSymbol,
    createJITDylib,
    getJITDylibByName,
    addDynamicLibrarySearchGeneratorForCurrentProcess,
    addDynamicLibrarySearchGenerator,
    ThreadSafeContext,
    withThreadSafeContext,
    createThreadSafeContext,
    disposeThreadSafeContext,
    withThreadSafeModule,
    createThreadSafeModule,
    disposeThreadSafeModule,
    ObjectLayer,
    createRTDyldObjectLinkingLayer,
    disposeObjectLayer,
    withRTDyldObjectLinkingLayer,
    IRLayer,
    createIRCompileLayer,
    disposeIRCompileLayer,
    LLVM.Internal.OrcJITV2.addModule,
  ) where

import LLVM.Internal.OrcJIT
import LLVM.Internal.OrcJIT.CompileLayer
import LLVM.Internal.OrcJIT.LinkingLayer
import LLVM.Internal.OrcJIT.CompileOnDemandLayer
import LLVM.Internal.OrcJIT.IRCompileLayer
import LLVM.Internal.OrcJIT.IRTransformLayer
import LLVM.Internal.OrcJITV2
