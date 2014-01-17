{-# LANGUAGE
  ForeignFunctionInterface
  #-}

-- | Functions to read and write LLVM bitcode
module LLVM.General.Internal.FFI.Bitcode where

import LLVM.General.Internal.FFI.Context 
import LLVM.General.Internal.FFI.MemoryBuffer
import LLVM.General.Internal.FFI.Module
import LLVM.General.Internal.FFI.LLVMCTypes

import Foreign.C
import Foreign.Ptr
import Control.Exception (bracket)

foreign import ccall unsafe "LLVM_General_ParseBitcode" parseBitcode ::
  Ptr Context -> Ptr MemoryBuffer -> Ptr (OwnerTransfered CString) -> IO (Ptr Module)

foreign import ccall unsafe "LLVM_General_WriteBitcodeToFile" writeBitcodeToFile ::
  Ptr Module -> CString -> Ptr (OwnerTransfered CString) -> IO LLVMBool

type ByteRangeCallback = Ptr CChar -> CSize -> IO ()
foreign import ccall "wrapper" wrapByteCallback :: 
  ByteRangeCallback -> IO (FunPtr ByteRangeCallback)

foreign import ccall "LLVM_General_GetModuleBitcode" getModuleBitcode' ::
  Ptr Module -> FunPtr ByteRangeCallback -> IO ()

getModuleBitcode :: Ptr Module -> ByteRangeCallback -> IO ()
getModuleBitcode m c = bracket (wrapByteCallback c) freeHaskellFunPtr (getModuleBitcode' m)

                               

