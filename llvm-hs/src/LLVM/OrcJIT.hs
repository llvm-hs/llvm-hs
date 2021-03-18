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
    -- ** Symbol lookups
    lookupSymbol,
    JITSymbol(..),
    JITSymbolError(..),
    JITSymbolFlags(..),
    defaultJITSymbolFlags,
    -- * ThreadSafeContext
    ThreadSafeContext,
    -- ** Lifetime management
    withThreadSafeContext,
    createThreadSafeContext,
    disposeThreadSafeContext,
    -- * ThreadSafeModule
    ThreadSafeModule,
    -- ** Lifetime management
    withThreadSafeModule,
    createThreadSafeModule,
    disposeThreadSafeModule,
    -- * Object layers
    ObjectLayer,
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
