{-# LANGUAGE
  ForeignFunctionInterface,
  MultiParamTypeClasses
  #-}

-- | <http://llvm.org/doxygen/classllvm_1_1BasicBlock.html>
module LLVM.General.Internal.FFI.BasicBlock where

import LLVM.General.Prelude

import Foreign.Ptr

import LLVM.General.Internal.FFI.PtrHierarchy

-- | <http://llvm.org/doxygen/group__LLVMCCoreValueBasicBlock.html#gab57c996ff697ef40966432055ae47a4e>
foreign import ccall unsafe "LLVMIsABasicBlock" isABasicBlock ::
    Ptr Value -> IO (Ptr BasicBlock)

-- | <http://llvm.org/doxygen/group__LLVMCCoreValueBasicBlock.html#ga754e45f69f4b784b658d9e379943f354>
foreign import ccall unsafe "LLVMGetBasicBlockTerminator" getBasicBlockTerminator ::
    Ptr BasicBlock -> IO (Ptr Instruction)

-- | <http://llvm.org/doxygen/group__LLVMCCoreValueBasicBlock.html#ga9baf824cd325ad211027b23fce8a7494>
foreign import ccall unsafe "LLVMGetFirstInstruction" getFirstInstruction ::
    Ptr BasicBlock -> IO (Ptr Instruction)

-- | <http://llvm.org/doxygen/group__LLVMCCoreValueBasicBlock.html#gaa0bb2c95802d06bf94f4c55e61fc3477>
foreign import ccall unsafe "LLVMGetLastInstruction" getLastInstruction ::
    Ptr BasicBlock -> IO (Ptr Instruction)

-- | <http://llvm.org/doxygen/group__LLVMCCoreValueInstruction.html#ga1b4c3bd197e86e8bffdda247ddf8ec5e>
foreign import ccall unsafe "LLVMGetNextInstruction" getNextInstruction ::
    Ptr Instruction -> IO (Ptr Instruction)

