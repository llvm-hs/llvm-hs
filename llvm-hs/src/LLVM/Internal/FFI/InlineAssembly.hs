{-# LANGUAGE
  ForeignFunctionInterface
  #-}

module LLVM.Internal.FFI.InlineAssembly where

import LLVM.Prelude

import Foreign.C
import Foreign.Ptr

import LLVM.Internal.FFI.LLVMCTypes
import LLVM.Internal.FFI.PtrHierarchy

foreign import ccall unsafe "LLVMIsAInlineAsm" isAInlineAsm ::
  Ptr Value -> IO (Ptr InlineAsm)

foreign import ccall unsafe "LLVM_Hs_CreateInlineAsm" createInlineAsm ::
  Ptr Type -> CString -> CString -> LLVMBool -> LLVMBool -> AsmDialect -> IO (Ptr InlineAsm)

foreign import ccall unsafe "LLVM_Hs_GetInlineAsmAsmString" getInlineAsmAssemblyString ::
  Ptr InlineAsm -> IO CString

foreign import ccall unsafe "LLVM_Hs_GetInlineAsmConstraintString" getInlineAsmConstraintString ::
  Ptr InlineAsm -> IO CString

foreign import ccall unsafe "LLVM_Hs_InlineAsmHasSideEffects" inlineAsmHasSideEffects ::
  Ptr InlineAsm -> IO LLVMBool

foreign import ccall unsafe "LLVM_Hs_InlineAsmIsAlignStack" inlineAsmIsAlignStack ::
  Ptr InlineAsm -> IO LLVMBool

foreign import ccall unsafe "LLVM_Hs_GetInlineAsmDialect" getInlineAsmDialect ::
  Ptr InlineAsm -> IO AsmDialect

