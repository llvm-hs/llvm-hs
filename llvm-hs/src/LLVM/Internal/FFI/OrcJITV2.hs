module LLVM.Internal.FFI.OrcJITV2 where

import LLVM.Prelude

import LLVM.Internal.FFI.DataLayout (DataLayout)
import LLVM.Internal.FFI.Module (Module)
import LLVM.Internal.FFI.OrcJIT (ExecutionSession, JITDylib)
import LLVM.Internal.FFI.Target (TargetMachine)

import Foreign.Ptr
import Foreign.C

data LLVMJITEvaluatedSymbol
data JITEvaluatedSymbol
data ThreadSafeContext
data ThreadSafeModule
data ObjectLayer
data IRLayer

foreign import ccall safe "LLVM_Hs_createThreadSafeContext" createThreadSafeContext ::
  IO (Ptr ThreadSafeContext)

foreign import ccall safe "LLVM_Hs_disposeThreadSafeContext" disposeThreadSafeContext ::
  Ptr ThreadSafeContext -> IO ()

foreign import ccall safe "LLVM_Hs_createThreadSafeModule" createThreadSafeModule ::
  Ptr Module -> IO (Ptr ThreadSafeModule)

foreign import ccall safe "LLVM_Hs_disposeThreadSafeModule" disposeThreadSafeModule ::
  Ptr ThreadSafeModule -> IO ()

foreign import ccall safe "LLVM_Hs_createRTDyldObjectLinkingLayer" createRTDyldObjectLinkingLayer ::
  Ptr ExecutionSession -> IO (Ptr ObjectLayer)

foreign import ccall safe "LLVM_Hs_disposeObjectLayer" disposeObjectLayer ::
  Ptr ObjectLayer -> IO ()

foreign import ccall safe "LLVM_Hs_createIRCompileLayer" createIRCompileLayer ::
  Ptr ExecutionSession -> Ptr ObjectLayer -> Ptr TargetMachine -> IO (Ptr IRLayer)

foreign import ccall safe "LLVM_Hs_disposeIRLayer" disposeIRLayer ::
  Ptr IRLayer -> IO ()

foreign import ccall safe "LLVM_Hs_ExecutionSession_createJITDylib" createJITDylib ::
  Ptr ExecutionSession -> CString -> IO (Ptr JITDylib)

foreign import ccall safe "LLVM_Hs_ExecutionSession_getJITDylibByName" getJITDylibByName ::
  Ptr ExecutionSession -> CString -> IO (Ptr JITDylib)

foreign import ccall safe "LLVM_Hs_JITDylib_addDynamicLibrarySearchGenerator_forCurrentProcess"
  addDynamicLibrarySearchGeneratorForCurrentProcess ::
    Ptr JITDylib -> Ptr DataLayout -> IO ()

foreign import ccall safe "LLVM_Hs_JITDylib_addDynamicLibrarySearchGenerator_load"
  addDynamicLibrarySearchGenerator ::
    Ptr JITDylib -> Ptr DataLayout -> CString -> IO ()

foreign import ccall safe "LLVM_Hs_IRLayer_addModule" irLayerAddModule ::
  Ptr ThreadSafeModule -> Ptr JITDylib -> Ptr DataLayout -> Ptr IRLayer -> IO ()

foreign import ccall safe "LLVM_Hs_ExecutionSession_lookup" lookupSymbol ::
  Ptr ExecutionSession -> Ptr JITDylib -> CString -> IO WordPtr

-- foreign import ccall safe "LLVM_Hs_ExecutionSession_lookup" lookupSymbol' ::
--   -- Ptr ExecutionSession -> Ptr JITDylib -> CString -> IO JITEvaluatedSymbol
--   Ptr ExecutionSession -> Ptr JITDylib -> CString -> IO LLVMJITEvaluatedSymbol
