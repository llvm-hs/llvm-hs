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

foreign import ccall unsafe "LLVM_General_IsAMDString" isAMDString ::
  Ptr Metadata -> IO (Ptr MDString)

foreign import ccall unsafe "LLVM_General_IsAMDNode" isAMDNode ::
  Ptr Metadata -> IO (Ptr MDNode)

foreign import ccall unsafe "LLVM_General_IsAMDValue" isAMDValue ::
  Ptr Metadata -> IO (Ptr MDValue)

foreign import ccall unsafe "LLVM_General_IsAMetadataOperand" isAMetadataOperand ::
  Ptr Value -> IO (Ptr MetadataAsVal)

foreign import ccall unsafe "LLVM_General_GetMDValue" getMDValue ::
  Ptr MDValue -> IO (Ptr Value)

foreign import ccall unsafe "LLVM_General_GetMetadataOperand" getMetadataOperand ::
  Ptr MetadataAsVal -> IO (Ptr Metadata)

foreign import ccall unsafe "LLVMGetMDKindIDInContext" getMDKindIDInContext' ::
  Ptr Context -> Ptr CChar -> CUInt -> IO MDKindID

getMDKindIDInContext ctx (c, n) = getMDKindIDInContext' ctx c n

foreign import ccall unsafe "LLVM_General_GetMDKindNames" getMDKindNames ::
  Ptr Context -> Ptr (Ptr CChar) -> Ptr CUInt -> CUInt -> IO CUInt

foreign import ccall unsafe "LLVM_General_MDStringInContext" mdStringInContext' ::
  Ptr Context -> CString -> CUInt -> IO (Ptr MDString)

foreign import ccall unsafe "LLVM_General_MDValue" mdValue ::
  Ptr Value -> IO (Ptr MDValue)

foreign import ccall unsafe "LLVM_General_MetadataOperand" metadataOperand ::
  Ptr Context -> Ptr Metadata -> IO (Ptr Value)

mdStringInContext ctx (p, n) = mdStringInContext' ctx p n

foreign import ccall unsafe "LLVM_General_GetMDString" getMDString ::
  Ptr MDString -> Ptr CUInt -> IO CString

foreign import ccall unsafe "LLVM_General_MDNodeInContext" createMDNodeInContext' ::
  Ptr Context -> Ptr (Ptr Metadata) -> CUInt -> IO (Ptr MDNode)

createMDNodeInContext ctx (n, vs) = createMDNodeInContext' ctx vs n

foreign import ccall unsafe "LLVM_General_CreateTemporaryMDNodeInContext" createTemporaryMDNodeInContext ::
  Ptr Context -> IO (Ptr MDNode)

foreign import ccall unsafe "LLVM_General_DestroyTemporaryMDNode" destroyTemporaryMDNode ::
  Ptr MDNode -> IO ()

foreign import ccall unsafe "LLVM_General_GetMDNodeNumOperands" getMDNodeNumOperands ::
  Ptr MDNode -> IO CUInt

foreign import ccall unsafe "LLVM_General_GetMDNodeOperands" getMDNodeOperands ::
  Ptr MDNode -> Ptr (Ptr Metadata) -> IO ()

foreign import ccall unsafe "LLVM_General_GetNamedMetadataName" getNamedMetadataName ::
  Ptr NamedMetadata -> Ptr CUInt -> IO (Ptr CChar)

foreign import ccall unsafe "LLVM_General_GetNamedMetadataNumOperands" getNamedMetadataNumOperands ::
  Ptr NamedMetadata -> IO CUInt

foreign import ccall unsafe "LLVM_General_GetNamedMetadataOperands" getNamedMetadataOperands ::
  Ptr NamedMetadata -> Ptr (Ptr MDNode) -> IO ()

foreign import ccall unsafe "LLVM_General_NamedMetadataAddOperands" namedMetadataAddOperands' ::
  Ptr NamedMetadata -> Ptr (Ptr MDNode) -> CUInt -> IO ()

foreign import ccall unsafe "LLVM_General_MetadataReplaceAllUsesWith" metadataReplaceAllUsesWith ::
  Ptr MDNode -> Ptr Metadata -> IO ()

namedMetadataAddOperands nm (n, vs) = namedMetadataAddOperands' nm vs n
