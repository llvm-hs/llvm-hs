{-# LANGUAGE ForeignFunctionInterface, MultiParamTypeClasses #-}

module LLVM.Internal.FFI.OrcJIT.LinkingLayer where

import LLVM.Prelude

import Foreign.C
import Foreign.Ptr

import LLVM.Internal.FFI.OrcJIT
import LLVM.Internal.FFI.LLVMCTypes
import LLVM.Internal.FFI.PtrHierarchy
import LLVM.Internal.FFI.ObjectFile

data LinkingLayer
data ObjectLinkingLayer
instance ChildOf LinkingLayer ObjectLinkingLayer

newtype ObjectHandle = ObjectHandle Word

foreign import ccall safe "LLVM_Hs_createObjectLinkingLayer" createObjectLinkingLayer ::
  IO (Ptr ObjectLinkingLayer)

foreign import ccall safe "LLVM_Hs_LinkingLayer_dispose" disposeLinkingLayer ::
  Ptr LinkingLayer -> IO ()

foreign import ccall safe "LLVM_Hs_LinkingLayer_addObject" addObjectFile ::
  Ptr LinkingLayer ->
  Ptr ObjectFile ->
  Ptr LambdaResolver ->
  Ptr (OwnerTransfered CString) ->
  IO ObjectHandle

foreign import ccall safe "LLVM_Hs_LinkingLayer_findSymbol" findSymbol ::
  Ptr LinkingLayer ->
  CString ->
  LLVMBool ->
  IO (Ptr JITSymbol)

foreign import ccall safe "LLVM_Hs_LinkingLayer_findSymbolIn" findSymbolIn ::
  Ptr LinkingLayer ->
  ObjectHandle ->
  CString ->
  LLVMBool ->
  IO (Ptr JITSymbol)
