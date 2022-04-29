{-# LANGUAGE ForeignFunctionInterface #-}

module LLVM.Internal.FFI.Passes where

import LLVM.Prelude

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

import LLVM.Internal.FFI.Error
import LLVM.Internal.FFI.Module
import LLVM.Internal.FFI.Target


data PassBuilderOptions

foreign import ccall unsafe "LLVMCreatePassBuilderOptions" createPassBuilderOptions :: IO (Ptr PassBuilderOptions)
foreign import ccall unsafe "LLVMDisposePassBuilderOptions" disposePassBuilderOptions :: Ptr PassBuilderOptions -> IO ()

foreign import ccall unsafe "LLVMRunPasses" runPasses
  :: Ptr Module -> CString -> Ptr TargetMachine -> Ptr PassBuilderOptions -> IO (Ptr Error)

data PassBuilderPackage
data ModulePassManager
data ModulePass

foreign import ccall unsafe "LLVM_Hs_CreatePassBuilderPackage" createPassBuilderPackage
  :: Ptr TargetMachine -> IO (Ptr PassBuilderPackage)

foreign import ccall unsafe "LLVM_Hs_DisposePassBuilderPackage" disposePassBuilderPackage
  :: Ptr PassBuilderPackage -> IO ()

foreign import ccall unsafe "LLVM_Hs_CreateModulePassManager" createModulePassManager
  :: IO (Ptr ModulePassManager)

foreign import ccall unsafe "LLVM_Hs_DisposeModulePassManager" disposeModulePassManager
  :: Ptr ModulePassManager -> IO ()

foreign import ccall unsafe "LLVM_Hs_ModulePassManagerRun" modulePassManagerRun
  :: Ptr ModulePassManager -> Ptr PassBuilderPackage -> Ptr Module -> IO ()

foreign import ccall unsafe "LLVM_Hs_AddPerModuleDefaultPipeline" addPerModuleDefaultPipeline
  :: Ptr ModulePassManager -> Ptr PassBuilderPackage -> CInt -> IO ()

foreign import ccall unsafe "LLVM_Hs_AddGlobalDeadCodeEliminationPass" addGlobalDeadCodeEliminationPass
  :: Ptr ModulePassManager -> IO ()

foreign import ccall unsafe "LLVM_Hs_AddAlwaysInlinePass" addAlwaysInlinePass
  :: Ptr ModulePassManager -> CInt -> IO ()

foreign import ccall unsafe "LLVM_Hs_AddInternalizeFunctionsPass" addInternalizeFunctionsPass
  :: Ptr ModulePassManager -> CInt -> Ptr CString -> IO ()
