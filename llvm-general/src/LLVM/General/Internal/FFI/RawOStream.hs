{-# LANGUAGE
  ForeignFunctionInterface
  #-}
module LLVM.General.Internal.FFI.RawOStream where

import LLVM.General.Prelude

import Foreign.Ptr
import Foreign.C
import Control.Exception (bracket)

import LLVM.General.Internal.FFI.ByteRangeCallback
import LLVM.General.Internal.FFI.LLVMCTypes

data RawOStream

type RawOStreamCallback = Ptr RawOStream -> IO ()
foreign import ccall "wrapper" wrapRawOStreamCallback ::
  RawOStreamCallback -> IO (FunPtr RawOStreamCallback)

foreign import ccall safe "LLVM_General_WithFileRawOStream" withFileRawOStream' ::
  CString -> LLVMBool -> LLVMBool -> Ptr (OwnerTransfered CString) -> FunPtr RawOStreamCallback -> IO LLVMBool
 
withFileRawOStream :: CString -> LLVMBool -> LLVMBool -> Ptr (OwnerTransfered CString) -> RawOStreamCallback -> IO LLVMBool
withFileRawOStream p ex bin err c = 
  bracket (wrapRawOStreamCallback c) freeHaskellFunPtr (withFileRawOStream' p ex bin err)

foreign import ccall safe "LLVM_General_WithBufferRawOStream" withBufferRawOStream' ::
  FunPtr ByteRangeCallback -> FunPtr RawOStreamCallback -> IO ()

withBufferRawOStream :: ByteRangeCallback -> RawOStreamCallback -> IO ()
withBufferRawOStream oc c = 
  bracket (wrapRawOStreamCallback c) freeHaskellFunPtr $ \c -> 
  bracket (wrapByteRangeCallback oc) freeHaskellFunPtr $ \oc ->
    withBufferRawOStream' oc c
