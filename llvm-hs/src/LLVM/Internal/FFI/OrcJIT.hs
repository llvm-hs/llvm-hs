module LLVM.Internal.FFI.OrcJIT where

import LLVM.Prelude

import LLVM.Internal.FFI.DataLayout (DataLayout)
import LLVM.Internal.FFI.Module (Module)
import LLVM.Internal.FFI.Target (TargetMachine)
import LLVM.Internal.FFI.LLVMCTypes

import Foreign.Ptr
import Foreign.C

newtype TargetAddress = TargetAddress Word64

data LLVMJITEvaluatedSymbol
data ExpectedJITEvaluatedSymbol
data MangleAndInterner
data ThreadSafeContext
data ThreadSafeModule
data ObjectLayer
data IRLayer
data JITDylib
data ExecutionSession

foreign import ccall safe "LLVM_Hs_createExecutionSession" createExecutionSession ::
  IO (Ptr ExecutionSession)

foreign import ccall safe "LLVM_Hs_disposeExecutionSession" disposeExecutionSession ::
  Ptr ExecutionSession -> IO ()

foreign import ccall safe "LLVM_Hs_ExecutionSession_endSession" endSession ::
  Ptr ExecutionSession -> IO ()

foreign import ccall safe "LLVM_Hs_ExecutionSession_createJITDylib" createJITDylib ::
  Ptr ExecutionSession -> CString -> IO (Ptr JITDylib)

foreign import ccall safe "LLVM_Hs_ExecutionSession_lookupSymbol" lookupSymbol ::
  Ptr ExecutionSession -> Ptr JITDylib -> Ptr MangleAndInterner -> CString -> IO (Ptr ExpectedJITEvaluatedSymbol)

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

foreign import ccall safe "LLVM_Hs_JITDylib_addDynamicLibrarySearchGenerator_forCurrentProcess"
  addDynamicLibrarySearchGeneratorForCurrentProcess ::
    Ptr JITDylib -> Ptr DataLayout -> IO ()

foreign import ccall safe "LLVM_Hs_JITDylib_addDynamicLibrarySearchGenerator_load"
  addDynamicLibrarySearchGenerator ::
    Ptr JITDylib -> Ptr DataLayout -> CString -> IO ()

foreign import ccall safe "LLVM_Hs_IRLayer_addModule" irLayerAddModule ::
  Ptr ThreadSafeModule -> Ptr JITDylib -> Ptr DataLayout -> Ptr IRLayer -> IO ()

foreign import ccall safe "LLVM_Hs_getExpectedJITEvaluatedSymbolAddress" getExpectedSymbolAddress ::
  Ptr ExpectedJITEvaluatedSymbol -> Ptr (OwnerTransfered CString) -> IO TargetAddress

foreign import ccall safe "LLVM_Hs_getExpectedJITEvaluatedSymbolFlags" getExpectedSymbolFlags ::
  Ptr ExpectedJITEvaluatedSymbol -> IO JITSymbolFlags

foreign import ccall safe "LLVM_Hs_disposeJITEvaluatedSymbol" disposeJITEvaluatedSymbol ::
  Ptr ExpectedJITEvaluatedSymbol -> IO ()

foreign import ccall safe "LLVM_Hs_createMangleAndInterner" createMangleAndInterner ::
  Ptr ExecutionSession -> Ptr DataLayout -> IO (Ptr MangleAndInterner)

foreign import ccall safe "LLVM_Hs_disposeMangleAndInterner" disposeMangleAndInterner ::
  Ptr MangleAndInterner -> IO ()
