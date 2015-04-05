{-#
  LANGUAGE
  ForeignFunctionInterface
  #-}

module LLVM.General.Internal.FFI.Metadata where

import LLVM.General.Prelude

import Foreign.Ptr
import Foreign.C

import LLVM.General.Internal.FFI.Context
import LLVM.General.Internal.FFI.PtrHierarchy
import LLVM.General.Internal.FFI.LLVMCTypes

foreign import ccall unsafe "LLVMIsAMDString" isAMDString ::
  Ptr Value -> IO (Ptr MDString)

foreign import ccall unsafe "LLVMIsAMDNode" isAMDNode ::
  Ptr Value -> IO (Ptr MDNode)

foreign import ccall unsafe "LLVMGetMDKindIDInContext" getMDKindIDInContext' ::
  Ptr Context -> Ptr CChar -> CUInt -> IO MDKindID

getMDKindIDInContext ctx (c, n) = getMDKindIDInContext' ctx c n

foreign import ccall unsafe "LLVM_General_GetMDKindNames" getMDKindNames ::
  Ptr Context -> Ptr (Ptr CChar) -> Ptr CUInt -> CUInt -> IO CUInt

foreign import ccall unsafe "LLVMMDStringInContext" mdStringInContext' ::
  Ptr Context -> CString -> CUInt -> IO (Ptr MDString)

mdStringInContext ctx (p, n) = mdStringInContext' ctx p n

foreign import ccall unsafe "LLVMGetMDString" getMDString ::
  Ptr MDString -> Ptr CUInt -> IO CString

foreign import ccall unsafe "LLVMMDNodeInContext" createMDNodeInContext' ::
  Ptr Context -> Ptr (Ptr Value) -> CUInt -> IO (Ptr MDNode)

createMDNodeInContext ctx (n, vs) = createMDNodeInContext' ctx vs n

foreign import ccall unsafe "LLVM_General_CreateTemporaryMDNodeInContext" createTemporaryMDNodeInContext ::
  Ptr Context -> IO (Ptr MDNode)

foreign import ccall unsafe "LLVM_General_DestroyTemporaryMDNode" destroyTemporaryMDNode ::
  Ptr MDNode -> IO ()

foreign import ccall unsafe "LLVM_General_GetMDNodeNumOperands" getMDNodeNumOperands ::
  Ptr MDNode -> IO CUInt

foreign import ccall unsafe "LLVMGetMDNodeOperands" getMDNodeOperands ::
  Ptr MDNode -> Ptr (Ptr Value) -> IO ()

foreign import ccall unsafe "LLVM_General_MDNodeIsFunctionLocal" mdNodeIsFunctionLocal ::
  Ptr MDNode -> IO LLVMBool

foreign import ccall unsafe "LLVM_General_GetNamedMetadataName" getNamedMetadataName ::
  Ptr NamedMetadata -> Ptr CUInt -> IO (Ptr CChar)

foreign import ccall unsafe "LLVM_General_GetNamedMetadataNumOperands" getNamedMetadataNumOperands ::
  Ptr NamedMetadata -> IO CUInt

foreign import ccall unsafe "LLVM_General_GetNamedMetadataOperands" getNamedMetadataOperands ::
  Ptr NamedMetadata -> Ptr (Ptr MDNode) -> IO ()

foreign import ccall unsafe "LLVM_General_NamedMetadataAddOperands" namedMetadataAddOperands' ::
  Ptr NamedMetadata -> Ptr (Ptr MDNode) -> CUInt -> IO ()

namedMetadataAddOperands nm (n, vs) = namedMetadataAddOperands' nm vs n
