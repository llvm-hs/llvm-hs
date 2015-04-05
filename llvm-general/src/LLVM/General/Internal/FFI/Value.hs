{-# LANGUAGE
  ForeignFunctionInterface,
  MultiParamTypeClasses,
  UndecidableInstances
  #-}
-- | FFI functions for handling the LLVM Value class
module LLVM.General.Internal.FFI.Value where

import LLVM.General.Prelude

import Foreign.Ptr
import Foreign.C

import LLVM.General.Internal.FFI.LLVMCTypes
import LLVM.General.Internal.FFI.PtrHierarchy

-- | <http://llvm.org/doxygen/group__LLVMCCoreValueGeneral.html#ga12179f46b79de8436852a4189d4451e0>
foreign import ccall unsafe "LLVMTypeOf" typeOf ::
  Ptr Value -> IO (Ptr Type)

-- | <http://llvm.org/doxygen/group__LLVMCCoreValueGeneral.html#ga70948786725c43968d15225dd584e5a9>
foreign import ccall unsafe "LLVMGetValueName" getValueName ::
  Ptr Value -> IO CString

-- | <http://llvm.org/doxygen/group__LLVMCCoreValueGeneral.html#gac1f61f74d83d218d4943c018e8fd8d13>
foreign import ccall unsafe "LLVMSetValueName" setValueName ::
  Ptr Value -> CString -> IO ()

-- | This function exposes the ID returned by llvm::Value::getValueID()
-- | <http://llvm.org/doxygen/classllvm_1_1Value.html#a2983b7b4998ef5b9f51b18c01588af3c>. 
foreign import ccall unsafe "LLVM_General_GetValueSubclassId" getValueSubclassId ::
  Ptr Value -> IO ValueSubclassId

foreign import ccall unsafe "LLVMReplaceAllUsesWith" replaceAllUsesWith ::
  Ptr Value -> Ptr Value -> IO ()

foreign import ccall unsafe "LLVM_General_CreateArgument" createArgument ::
  Ptr Type -> CString -> IO (Ptr Value)

foreign import ccall unsafe "LLVMDumpValue" dumpValue ::
  Ptr Value -> IO ()
