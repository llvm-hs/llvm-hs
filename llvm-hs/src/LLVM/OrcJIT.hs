module LLVM.OrcJIT (
    -- * ExecutionSession
    ExecutionSession,
    createExecutionSession,
    disposeExecutionSession,
    withExecutionSession,
    -- * JITDylib
    JITDylib(..),
    createJITDylib,
    -- ** Symbol search generators
    addDynamicLibrarySearchGeneratorForCurrentProcess,
    addDynamicLibrarySearchGenerator,
    -- ** Symbol definitions
    defineAbsoluteSymbols,
    -- ** Symbol lookups
    lookupSymbol,
    JITSymbol(..),
    JITSymbolError(..),
    JITSymbolFlags(..),
    defaultJITSymbolFlags,
    -- * Mangled symbols
    mangleSymbol,
    disposeMangledSymbol,
    withMangledSymbol,
    -- * ThreadSafeContext
    ThreadSafeContext,
    -- ** Lifetime management
    withThreadSafeContext,
    createThreadSafeContext,
    disposeThreadSafeContext,
    -- * ThreadSafeModule
    ThreadSafeModule(..),
    -- ** Lifetime management
    withClonedThreadSafeModule,
    cloneAsThreadSafeModule,
    disposeThreadSafeModule,
    -- * Object layers
    ObjectLayer,
    addObjectFile,
    -- ** ObjectLinkingLayer
    ObjectLinkingLayer,
    createObjectLinkingLayer,
    -- ** RTDyldObjectLinkingLayer
    RTDyldObjectLinkingLayer,
    createRTDyldObjectLinkingLayer,
    -- * IR layers
    IRLayer,
    addModule,
    -- ** IRCompileLayer
    IRCompileLayer,
    createIRCompileLayer,
  ) where

import LLVM.Internal.OrcJIT
