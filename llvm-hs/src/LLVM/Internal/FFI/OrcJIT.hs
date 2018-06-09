{-# LANGUAGE MultiParamTypeClasses, ForeignFunctionInterface #-}

module LLVM.Internal.FFI.OrcJIT where

import LLVM.Prelude

import Foreign.C
import Foreign.Ptr

import LLVM.Internal.FFI.DataLayout
import LLVM.Internal.FFI.LLVMCTypes

data JITSymbol
data LambdaResolver

newtype TargetAddress = TargetAddress Word64

type SymbolResolverFn = CString -> Ptr JITSymbol -> IO ()

foreign import ccall "wrapper" wrapSymbolResolverFn ::
  SymbolResolverFn -> IO (FunPtr SymbolResolverFn)

foreign import ccall safe "LLVM_Hs_disposeJITSymbol" disposeSymbol ::
  Ptr JITSymbol -> IO ()

foreign import ccall safe "LLVM_Hs_createLambdaResolver" createLambdaResolver ::
  FunPtr SymbolResolverFn ->
  FunPtr SymbolResolverFn ->
  IO (Ptr LambdaResolver)

foreign import ccall safe "LLVM_Hs_disposeLambdaResolver" disposeLambdaResolver ::
  Ptr LambdaResolver -> IO ()

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
