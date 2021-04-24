module LLVM.Internal.FFI.OrcJITV2 where

import LLVM.Prelude

import LLVM.Internal.FFI.DataLayout (DataLayout)
import LLVM.Internal.FFI.Module (Module)
import LLVM.Internal.FFI.OrcJIT (ExecutionSession)
import LLVM.Internal.FFI.Target (TargetMachine)

import Foreign.Ptr
import Foreign.C

data ThreadSafeContext
data ObjectLayer
data IRLayer

foreign import ccall safe "LLVM_Hs_createThreadSafeContext" createThreadSafeContext ::
  IO (Ptr ThreadSafeContext)

foreign import ccall safe "LLVM_Hs_disposeThreadSafeContext" disposeThreadSafeContext ::
  Ptr ThreadSafeContext -> IO ()

foreign import ccall safe "LLVM_Hs_createRTDyldObjectLinkingLayer" createRTDyldObjectLinkingLayer ::
  Ptr ExecutionSession -> IO (Ptr ObjectLayer)

foreign import ccall safe "LLVM_Hs_disposeObjectLayer" disposeObjectLayer ::
  Ptr ObjectLayer -> IO ()

foreign import ccall safe "LLVM_Hs_createIRCompileLayer" createIRCompileLayer ::
  Ptr ExecutionSession -> Ptr ObjectLayer -> Ptr TargetMachine -> IO (Ptr IRLayer)

foreign import ccall safe "LLVM_Hs_disposeIRLayer" disposeIRLayer ::
  Ptr IRLayer -> IO ()

foreign import ccall safe "LLVM_Hs_IRLayer_add" irLayerAdd ::
  Ptr ThreadSafeContext -> Ptr ExecutionSession ->  Ptr DataLayout -> Ptr IRLayer -> Ptr Module -> IO ()

foreign import ccall safe "LLVM_Hs_ExecutionSession_lookup" esLookup ::
  Ptr ExecutionSession -> CString -> IO Word64
