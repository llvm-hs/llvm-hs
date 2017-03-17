{-# LANGUAGE
  ForeignFunctionInterface
  #-}
module LLVM.Internal.FFI.RawOStream where

import LLVM.Prelude

import Foreign.Ptr
import Foreign.C
import Control.Exception (bracket)

import LLVM.Internal.FFI.ByteRangeCallback
import LLVM.Internal.FFI.LLVMCTypes
import LLVM.Internal.FFI.PtrHierarchy

type RawPWriteStreamCallback = Ptr RawPWriteStream -> IO ()

foreign import ccall "wrapper" wrapRawPWriteStreamCallback ::
  RawPWriteStreamCallback -> IO (FunPtr RawPWriteStreamCallback)

foreign import ccall safe "LLVM_Hs_WithFileRawPWriteStream" withFileRawPWriteStream' ::
  CString -> LLVMBool -> LLVMBool -> Ptr (OwnerTransfered CString) -> FunPtr RawPWriteStreamCallback -> IO LLVMBool

withFileRawPWriteStream :: CString -> LLVMBool -> LLVMBool -> Ptr (OwnerTransfered CString) -> RawPWriteStreamCallback -> IO LLVMBool
withFileRawPWriteStream p ex bin err c =
  bracket (wrapRawPWriteStreamCallback c) freeHaskellFunPtr (withFileRawPWriteStream' p ex bin err)

foreign import ccall safe "LLVM_Hs_WithBufferRawPWriteStream" withBufferRawPWriteStream' ::
  FunPtr ByteRangeCallback -> FunPtr RawPWriteStreamCallback -> IO ()

withBufferRawPWriteStream :: ByteRangeCallback -> RawPWriteStreamCallback -> IO ()
withBufferRawPWriteStream oc c =
  bracket (wrapRawPWriteStreamCallback c) freeHaskellFunPtr $ \c ->
  bracket (wrapByteRangeCallback oc) freeHaskellFunPtr $ \oc ->
    withBufferRawPWriteStream' oc c
