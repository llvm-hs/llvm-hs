{-# LANGUAGE
  ForeignFunctionInterface
  #-}

module LLVM.Internal.FFI.ExecutionEngine where

import LLVM.Prelude

import Foreign.Ptr
import Foreign.C

import LLVM.Internal.FFI.PtrHierarchy
import LLVM.Internal.FFI.Module
import LLVM.Internal.FFI.LLVMCTypes

data ExecutionEngine

foreign import ccall unsafe "LLVMCreateExecutionEngineForModule" createExecutionEngineForModule ::
  Ptr (Ptr ExecutionEngine) -> Ptr Module -> Ptr (OwnerTransfered CString) -> IO CUInt

foreign import ccall unsafe "LLVMCreateInterpreterForModule" createInterpreterForModule ::
  Ptr (Ptr ExecutionEngine) -> Ptr Module -> Ptr (OwnerTransfered CString) -> IO CUInt

foreign import ccall unsafe "LLVMCreateJITCompilerForModule" createJITCompilerForModule ::
  Ptr (Ptr ExecutionEngine) -> Ptr Module -> CUInt -> Ptr (OwnerTransfered CString) -> IO CUInt

foreign import ccall unsafe "LLVMCreateMCJITCompilerForModule" createMCJITCompilerForModule ::
  Ptr (Ptr ExecutionEngine) -> Ptr Module -> Ptr MCJITCompilerOptions -> CSize -> Ptr (OwnerTransfered CString) -> IO CUInt

foreign import ccall unsafe "LLVMDisposeExecutionEngine" disposeExecutionEngine ::
  Ptr ExecutionEngine -> IO ()

foreign import ccall unsafe "LLVMAddModule" addModule ::
  Ptr ExecutionEngine -> Ptr Module -> IO ()

foreign import ccall unsafe "LLVMRemoveModule" removeModule ::
  Ptr ExecutionEngine -> Ptr Module -> Ptr (Ptr Module) -> Ptr CString -> IO CUInt

foreign import ccall unsafe "LLVMFindFunction" findFunction ::
  Ptr ExecutionEngine -> CString -> Ptr (Ptr Function) -> IO CUInt

foreign import ccall unsafe "LLVMGetPointerToGlobal" getPointerToGlobal ::
  Ptr ExecutionEngine -> Ptr GlobalValue -> IO (Ptr ())

foreign import ccall unsafe "LLVMLinkInInterpreter" linkInInterpreter :: 
  IO ()

foreign import ccall unsafe "LLVMLinkInMCJIT" linkInMCJIT :: 
  IO ()

data MCJITCompilerOptions

foreign import ccall unsafe "LLVM_Hs_GetMCJITCompilerOptionsSize" getMCJITCompilerOptionsSize ::
  IO CSize

foreign import ccall unsafe "LLVMInitializeMCJITCompilerOptions" initializeMCJITCompilerOptions ::
  Ptr MCJITCompilerOptions -> CSize -> IO ()

foreign import ccall unsafe "LLVM_Hs_SetMCJITCompilerOptionsOptLevel" setMCJITCompilerOptionsOptLevel ::
  Ptr MCJITCompilerOptions -> CUInt -> IO ()

foreign import ccall unsafe "LLVM_Hs_SetMCJITCompilerOptionsCodeModel" setMCJITCompilerOptionsCodeModel ::
  Ptr MCJITCompilerOptions -> CodeModel -> IO ()

foreign import ccall unsafe "LLVM_Hs_SetMCJITCompilerOptionsNoFramePointerElim" setMCJITCompilerOptionsNoFramePointerElim ::
  Ptr MCJITCompilerOptions -> LLVMBool -> IO ()

foreign import ccall unsafe "LLVM_Hs_SetMCJITCompilerOptionsEnableFastISel" setMCJITCompilerOptionsEnableFastISel ::
  Ptr MCJITCompilerOptions -> LLVMBool -> IO ()


