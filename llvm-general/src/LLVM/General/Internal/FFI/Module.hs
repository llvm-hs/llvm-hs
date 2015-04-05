{-# LANGUAGE
  ForeignFunctionInterface
  #-}
module LLVM.General.Internal.FFI.Module where

import LLVM.General.Prelude

import Foreign.Ptr
import Foreign.C

import LLVM.General.Internal.FFI.Context
import LLVM.General.Internal.FFI.LLVMCTypes
import LLVM.General.Internal.FFI.PtrHierarchy
import LLVM.General.Internal.FFI.Type

data Module

foreign import ccall unsafe "LLVMModuleCreateWithNameInContext" moduleCreateWithNameInContext ::
  CString -> Ptr Context -> IO (Ptr Module)

foreign import ccall unsafe "LLVMGetModuleContext" getModuleContext ::
  Ptr Module -> IO (Ptr Context)

foreign import ccall unsafe "LLVMDisposeModule" disposeModule ::
  Ptr Module -> IO ()

foreign import ccall unsafe "LLVMGetDataLayout" getDataLayout ::
  Ptr Module -> IO CString

foreign import ccall unsafe "LLVMSetDataLayout" setDataLayout ::
  Ptr Module -> CString -> IO ()

foreign import ccall unsafe "LLVMGetTarget" getTargetTriple ::
  Ptr Module -> IO CString

foreign import ccall unsafe "LLVMSetTarget" setTargetTriple ::
  Ptr Module -> CString -> IO ()

foreign import ccall unsafe "LLVM_General_GetModuleIdentifier" getModuleIdentifier ::
  Ptr Module -> IO (OwnerTransfered CString)

foreign import ccall unsafe "LLVMGetFirstGlobal" getFirstGlobal ::
  Ptr Module -> IO (Ptr GlobalVariable)

foreign import ccall unsafe "LLVMGetNextGlobal" getNextGlobal ::
  Ptr GlobalVariable -> IO (Ptr GlobalVariable)

foreign import ccall unsafe "LLVM_General_GetFirstAlias" getFirstAlias ::
  Ptr Module -> IO (Ptr GlobalAlias)

foreign import ccall unsafe "LLVM_General_GetNextAlias" getNextAlias ::
  Ptr GlobalAlias -> IO (Ptr GlobalAlias)

foreign import ccall unsafe "LLVMGetFirstFunction" getFirstFunction ::
  Ptr Module -> IO (Ptr Function)

foreign import ccall unsafe "LLVMGetNextFunction" getNextFunction ::
  Ptr Function -> IO (Ptr Function)

foreign import ccall unsafe "LLVM_General_GetFirstNamedMetadata" getFirstNamedMetadata ::
  Ptr Module -> IO (Ptr NamedMetadata)

foreign import ccall unsafe "LLVM_General_GetNextNamedMetadata" getNextNamedMetadata ::
  Ptr NamedMetadata -> IO (Ptr NamedMetadata)

foreign import ccall unsafe "LLVMAddGlobalInAddressSpace" addGlobalInAddressSpace ::
  Ptr Module -> Ptr Type -> CString -> CUInt -> IO (Ptr GlobalVariable)

foreign import ccall unsafe "LLVM_General_JustAddAlias" justAddAlias ::
  Ptr Module -> Ptr Type -> AddrSpace -> CString -> IO (Ptr GlobalAlias)


foreign import ccall unsafe "LLVMAddFunction" addFunction ::
  Ptr Module -> CString -> Ptr Type -> IO (Ptr Function)

foreign import ccall unsafe "LLVMGetNamedFunction" getNamedFunction ::
  Ptr Module -> CString -> IO (Ptr Function)

foreign import ccall unsafe "LLVM_General_GetOrAddNamedMetadata" getOrAddNamedMetadata ::
  Ptr Module -> CString -> IO (Ptr NamedMetadata)

foreign import ccall unsafe "LLVM_General_ModuleAppendInlineAsm" moduleAppendInlineAsm' ::
  Ptr Module -> Ptr CChar -> CUInt -> IO ()

newtype ModuleAsm a = ModuleAsm a

moduleAppendInlineAsm m (ModuleAsm (c, n)) = moduleAppendInlineAsm' m c n

foreign import ccall unsafe "LLVM_General_ModuleGetInlineAsm" moduleGetInlineAsm ::
  Ptr Module -> IO (ModuleAsm CString)

foreign import ccall unsafe "LLVMLinkModules" linkModules ::
  Ptr Module -> Ptr Module -> LinkerMode -> Ptr (OwnerTransfered CString) -> IO LLVMBool
