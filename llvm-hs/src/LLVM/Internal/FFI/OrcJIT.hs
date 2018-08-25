{-# LANGUAGE MultiParamTypeClasses, ForeignFunctionInterface #-}

module LLVM.Internal.FFI.OrcJIT where

import LLVM.Prelude

import Foreign.C
import Foreign.Ptr

import LLVM.Internal.FFI.DataLayout
import LLVM.Internal.FFI.LLVMCTypes

-- | Abstract type used as the identifier for a module.
newtype ModuleKey = ModuleKey Word64 deriving (Eq, Ord, Show)

data JITSymbol
data SymbolResolver
data ExecutionSession

newtype TargetAddress = TargetAddress Word64

type SymbolResolverFn = CString -> Ptr JITSymbol -> IO ()

foreign import ccall "wrapper" wrapSymbolResolverFn ::
  SymbolResolverFn -> IO (FunPtr SymbolResolverFn)

foreign import ccall safe "LLVM_Hs_disposeJITSymbol" disposeSymbol ::
  Ptr JITSymbol -> IO ()

foreign import ccall safe "LLVM_Hs_createLambdaResolver" createLambdaResolver ::
  Ptr ExecutionSession ->
  FunPtr SymbolResolverFn ->
  IO (Ptr SymbolResolver)

foreign import ccall safe "LLVM_Hs_disposeSymbolResolver" disposeSymbolResolver ::
  Ptr SymbolResolver -> IO ()

foreign import ccall safe "LLVM_Hs_JITSymbol_getAddress" getAddress ::
  Ptr JITSymbol -> Ptr (OwnerTransfered CString) -> IO TargetAddress

foreign import ccall safe "LLVM_Hs_JITSymbol_getFlags" getFlags ::
  Ptr JITSymbol -> IO JITSymbolFlags

foreign import ccall safe "LLVM_Hs_JITSymbol_getErrorMsg" getErrorMsg ::
  Ptr JITSymbol -> IO (OwnerTransfered CString)

foreign import ccall safe "LLVM_Hs_setJITSymbol" setJITSymbol ::
  Ptr JITSymbol -> TargetAddress -> JITSymbolFlags -> IO ()

foreign import ccall safe "LLVM_Hs_getMangledSymbol" getMangledSymbol ::
  Ptr CString -> CString -> Ptr DataLayout -> IO ()

foreign import ccall safe "LLVM_Hs_disposeMangledSymbol" disposeMangledSymbol ::
  CString -> IO ()

foreign import ccall safe "LLVM_Hs_createExecutionSession" createExecutionSession ::
  IO (Ptr ExecutionSession)

foreign import ccall safe "LLVM_Hs_disposeExecutionSession" disposeExecutionSession ::
  Ptr ExecutionSession -> IO ()

foreign import ccall safe "LLVM_Hs_allocateVModule" allocateVModule ::
  Ptr ExecutionSession -> IO ModuleKey

foreign import ccall safe "LLVM_Hs_releaseVModule" releaseVModule ::
  Ptr ExecutionSession -> ModuleKey -> IO ()

foreign import ccall "wrapper" wrapGetSymbolResolver ::
  (ModuleKey -> IO (Ptr SymbolResolver)) -> IO (FunPtr (ModuleKey -> IO (Ptr SymbolResolver)))

foreign import ccall "wrapper" wrapSetSymbolResolver ::
  (ModuleKey -> Ptr SymbolResolver -> IO ()) -> IO (FunPtr (ModuleKey -> Ptr SymbolResolver -> IO ()))
