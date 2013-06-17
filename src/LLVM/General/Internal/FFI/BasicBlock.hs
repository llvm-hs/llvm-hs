{-# LANGUAGE
  ForeignFunctionInterface,
  EmptyDataDecls,
  MultiParamTypeClasses
  #-}

module LLVM.General.Internal.FFI.BasicBlock where

import Foreign.Ptr

import LLVM.General.Internal.FFI.PtrHierarchy

foreign import ccall unsafe "LLVMIsABasicBlock" isABasicBlock ::
    Ptr Value -> IO (Ptr BasicBlock)

foreign import ccall unsafe "LLVMGetBasicBlockTerminator" getBasicBlockTerminator ::
    Ptr BasicBlock -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVMGetFirstInstruction" getFirstInstruction ::
    Ptr BasicBlock -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVMGetLastInstruction" getLastInstruction ::
    Ptr BasicBlock -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVMGetNextInstruction" getNextInstruction ::
    Ptr Instruction -> IO (Ptr Instruction)

