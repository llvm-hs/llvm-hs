{-#
  LANGUAGE
  ForeignFunctionInterface
  #-}

module LLVM.Internal.FFI.Metadata where

import LLVM.Prelude

import Foreign.Ptr
import Foreign.C

import LLVM.Internal.FFI.Context
import LLVM.Internal.FFI.PtrHierarchy
import LLVM.Internal.FFI.LLVMCTypes

foreign import ccall unsafe "LLVM_Hs_IsAMDString" isAMDString ::
  Ptr Metadata -> IO (Ptr MDString)

foreign import ccall unsafe "LLVM_Hs_IsAMDNode" isAMDNode ::
  Ptr Metadata -> IO (Ptr MDNode)

foreign import ccall unsafe "LLVM_Hs_IsAMDValue" isAMDValue ::
  Ptr Metadata -> IO (Ptr MDValue)

foreign import ccall unsafe "LLVM_Hs_IsAMetadataOperand" isAMetadataOperand ::
  Ptr Value -> IO (Ptr MetadataAsVal)

foreign import ccall unsafe "LLVM_Hs_GetMDValue" getMDValue ::
  Ptr MDValue -> IO (Ptr Value)

foreign import ccall unsafe "LLVM_Hs_GetMetadataOperand" getMetadataOperand ::
  Ptr MetadataAsVal -> IO (Ptr Metadata)

foreign import ccall unsafe "LLVMGetMDKindIDInContext" getMDKindIDInContext' ::
  Ptr Context -> Ptr CChar -> CUInt -> IO MDKindID

getMDKindIDInContext ctx (c, n) = getMDKindIDInContext' ctx c n

foreign import ccall unsafe "LLVM_Hs_GetMDKindNames" getMDKindNames ::
  Ptr Context -> Ptr (Ptr CChar) -> Ptr CUInt -> CUInt -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_MDStringInContext" mdStringInContext' ::
  Ptr Context -> CString -> CUInt -> IO (Ptr MDString)

foreign import ccall unsafe "LLVM_Hs_MDValue" mdValue ::
  Ptr Value -> IO (Ptr MDValue)

foreign import ccall unsafe "LLVM_Hs_MetadataOperand" metadataOperand ::
  Ptr Context -> Ptr Metadata -> IO (Ptr Value)

mdStringInContext ctx (p, n) = mdStringInContext' ctx p n

foreign import ccall unsafe "LLVM_Hs_GetMDString" getMDString ::
  Ptr MDString -> Ptr CUInt -> IO CString

foreign import ccall unsafe "LLVM_Hs_MDNodeInContext" createMDNodeInContext' ::
  Ptr Context -> Ptr (Ptr Metadata) -> CUInt -> IO (Ptr MDNode)

createMDNodeInContext ctx (n, vs) = createMDNodeInContext' ctx vs n

foreign import ccall unsafe "LLVM_Hs_CreateTemporaryMDNodeInContext" createTemporaryMDNodeInContext ::
  Ptr Context -> IO (Ptr MDNode)

foreign import ccall unsafe "LLVM_Hs_DestroyTemporaryMDNode" destroyTemporaryMDNode ::
  Ptr MDNode -> IO ()

foreign import ccall unsafe "LLVM_Hs_GetMDNodeNumOperands" getMDNodeNumOperands ::
  Ptr MDNode -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_GetMDNodeOperands" getMDNodeOperands ::
  Ptr MDNode -> Ptr (Ptr Metadata) -> IO ()

foreign import ccall unsafe "LLVM_Hs_GetNamedMetadataName" getNamedMetadataName ::
  Ptr NamedMetadata -> Ptr CUInt -> IO (Ptr CChar)

foreign import ccall unsafe "LLVM_Hs_GetNamedMetadataNumOperands" getNamedMetadataNumOperands ::
  Ptr NamedMetadata -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_GetNamedMetadataOperands" getNamedMetadataOperands ::
  Ptr NamedMetadata -> Ptr (Ptr MDNode) -> IO ()

foreign import ccall unsafe "LLVM_Hs_NamedMetadataAddOperands" namedMetadataAddOperands' ::
  Ptr NamedMetadata -> Ptr (Ptr MDNode) -> CUInt -> IO ()

foreign import ccall unsafe "LLVM_Hs_MetadataReplaceAllUsesWith" metadataReplaceAllUsesWith ::
  Ptr MDNode -> Ptr Metadata -> IO ()

namedMetadataAddOperands nm (n, vs) = namedMetadataAddOperands' nm vs n
