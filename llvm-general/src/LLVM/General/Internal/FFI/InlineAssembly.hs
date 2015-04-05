{-# LANGUAGE
  ForeignFunctionInterface
  #-}

module LLVM.General.Internal.FFI.InlineAssembly where

import LLVM.General.Prelude

import Foreign.C
import Foreign.Ptr

import LLVM.General.Internal.FFI.LLVMCTypes
import LLVM.General.Internal.FFI.PtrHierarchy

foreign import ccall unsafe "LLVMIsAInlineAsm" isAInlineAsm ::
  Ptr Value -> IO (Ptr InlineAsm)

foreign import ccall unsafe "LLVM_General_CreateInlineAsm" createInlineAsm ::
  Ptr Type -> CString -> CString -> LLVMBool -> LLVMBool -> AsmDialect -> IO (Ptr InlineAsm)

foreign import ccall unsafe "LLVM_General_GetInlineAsmAsmString" getInlineAsmAssemblyString ::
  Ptr InlineAsm -> IO CString

foreign import ccall unsafe "LLVM_General_GetInlineAsmConstraintString" getInlineAsmConstraintString ::
  Ptr InlineAsm -> IO CString

foreign import ccall unsafe "LLVM_General_InlineAsmHasSideEffects" inlineAsmHasSideEffects ::
  Ptr InlineAsm -> IO LLVMBool

foreign import ccall unsafe "LLVM_General_InlineAsmIsAlignStack" inlineAsmIsAlignStack ::
  Ptr InlineAsm -> IO LLVMBool

foreign import ccall unsafe "LLVM_General_GetInlineAsmDialect" getInlineAsmDialect ::
  Ptr InlineAsm -> IO AsmDialect

