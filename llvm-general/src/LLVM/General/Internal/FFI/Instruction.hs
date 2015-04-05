{-# LANGUAGE
  ForeignFunctionInterface,
  MultiParamTypeClasses,
  UndecidableInstances,
  TemplateHaskell
  #-}
module LLVM.General.Internal.FFI.Instruction where

import LLVM.General.Prelude

import Foreign.Ptr
import Foreign.C

import LLVM.General.Internal.FFI.PtrHierarchy
import LLVM.General.Internal.FFI.LLVMCTypes

foreign import ccall unsafe "LLVMIsAInstruction" isAInstruction ::
  Ptr Value -> IO (Ptr Instruction)

newtype COpcode = COpcode CUInt

-- get the C API opcode
foreign import ccall unsafe "LLVMGetInstructionOpcode" getInstructionOpcode :: 
  Ptr Instruction -> IO COpcode

-- get the C++ API opcode (one less level of mapping than for that from the C API)
foreign import ccall unsafe "LLVM_General_GetInstructionDefOpcode" getInstructionDefOpcode :: 
  Ptr Instruction -> IO CPPOpcode

foreign import ccall unsafe "LLVMGetICmpPredicate" getICmpPredicate ::
  Ptr Instruction -> IO ICmpPredicate

foreign import ccall unsafe "LLVM_General_GetFCmpPredicate" getFCmpPredicate ::
  Ptr Instruction -> IO FCmpPredicate

foreign import ccall unsafe "LLVMGetInstructionCallConv" getInstructionCallConv ::
  Ptr Instruction -> IO CallConv

foreign import ccall unsafe "LLVMSetInstructionCallConv" setInstructionCallConv ::
  Ptr Instruction -> CallConv -> IO ()

foreign import ccall unsafe "LLVMIsTailCall" isTailCall ::
  Ptr Instruction -> IO LLVMBool

foreign import ccall unsafe "LLVMSetTailCall" setTailCall ::
  Ptr Instruction -> LLVMBool -> IO ()

foreign import ccall unsafe "LLVM_General_GetCallInstCalledValue" getCallInstCalledValue ::
  Ptr Instruction -> IO (Ptr Value)

foreign import ccall unsafe "LLVM_General_GetCallInstFunctionAttr" getCallInstFunctionAttr ::
  Ptr Instruction -> IO FunctionAttr

foreign import ccall unsafe "LLVM_General_AddCallInstFunctionAttr" addCallInstFunctionAttr ::
  Ptr Instruction -> FunctionAttr -> IO ()

foreign import ccall unsafe "LLVM_General_GetCallInstAttr" getCallInstAttr ::
  Ptr Instruction -> CUInt -> IO ParamAttr

foreign import ccall unsafe "LLVM_General_AddCallInstAttr" addCallInstAttr ::
  Ptr Instruction -> CUInt -> ParamAttr -> IO ()

foreign import ccall unsafe "LLVMAddIncoming" addIncoming' ::
  Ptr Instruction -> Ptr (Ptr Value) -> Ptr (Ptr BasicBlock) -> CUInt -> IO ()

addIncoming :: Ptr Instruction -> (CUInt, Ptr (Ptr Value)) -> (CUInt, Ptr (Ptr BasicBlock)) -> IO ()
addIncoming i (nvs, vs) (nbs, bs) | nbs == nvs = addIncoming' i vs bs nbs

foreign import ccall unsafe "LLVMCountIncoming" countIncoming ::
  Ptr Instruction -> IO CUInt

foreign import ccall unsafe "LLVMGetIncomingValue" getIncomingValue ::
  Ptr Instruction -> CUInt -> IO (Ptr Value)

foreign import ccall unsafe "LLVMGetIncomingBlock" getIncomingBlock ::
  Ptr Instruction -> CUInt -> IO (Ptr BasicBlock)


foreign import ccall unsafe "LLVMAddCase" addCase ::
  Ptr Instruction -> Ptr Constant -> Ptr BasicBlock -> IO ()

foreign import ccall unsafe "LLVM_General_GetSwitchCases" getSwitchCases ::
  Ptr Instruction -> Ptr (Ptr Constant) -> Ptr (Ptr BasicBlock) -> IO ()

foreign import ccall unsafe "LLVMAddDestination" addDestination ::
  Ptr Instruction -> Ptr BasicBlock -> IO ()

foreign import ccall unsafe "LLVM_General_GetIndirectBrDests" getIndirectBrDests ::
  Ptr Instruction -> Ptr (Ptr BasicBlock) -> IO ()


foreign import ccall unsafe "LLVM_General_GetInstrAlignment" getInstrAlignment ::
  Ptr Instruction -> IO CUInt

foreign import ccall unsafe "LLVM_General_SetInstrAlignment" setInstrAlignment ::
  Ptr Instruction -> CUInt -> IO ()

foreign import ccall unsafe "LLVM_General_GetAllocaNumElements" getAllocaNumElements ::
  Ptr Instruction -> IO (Ptr Value)

foreign import ccall unsafe "LLVM_General_GetAllocatedType" getAllocatedType ::
  Ptr Instruction -> IO (Ptr Type)

foreign import ccall unsafe "LLVM_General_GetAtomicOrdering" getAtomicOrdering ::
  Ptr Instruction -> IO MemoryOrdering

foreign import ccall unsafe "LLVM_General_GetFailureAtomicOrdering" getFailureAtomicOrdering ::
  Ptr Instruction -> IO MemoryOrdering

foreign import ccall unsafe "LLVM_General_GetSynchronizationScope" getSynchronizationScope ::
  Ptr Instruction -> IO SynchronizationScope

getAtomicity i = return (,) `ap` getSynchronizationScope i `ap` getAtomicOrdering i

foreign import ccall unsafe "LLVM_General_GetVolatile" getVolatile ::
  Ptr Instruction -> IO LLVMBool

foreign import ccall unsafe "LLVM_General_GetInBounds" getInBounds ::
  Ptr Value -> IO LLVMBool

foreign import ccall unsafe "LLVM_General_GetAtomicRMWBinOp" getAtomicRMWBinOp ::
  Ptr Instruction -> IO RMWOperation

foreign import ccall unsafe "LLVM_General_CountInstStructureIndices" countInstStructureIndices ::
  Ptr Instruction -> IO CUInt

foreign import ccall unsafe "LLVM_General_GetInstStructureIndices" getInstStructureIndices ::
  Ptr Instruction -> Ptr CUInt -> IO ()

foreign import ccall unsafe "LLVMAddClause" addClause ::
  Ptr Instruction -> Ptr Constant -> IO ()

foreign import ccall unsafe "LLVMSetCleanup" setCleanup ::
  Ptr Instruction -> LLVMBool -> IO ()

foreign import ccall unsafe "LLVM_General_IsCleanup" isCleanup ::
  Ptr Instruction -> IO LLVMBool

foreign import ccall unsafe "LLVMSetMetadata" setMetadata ::
  Ptr Instruction -> MDKindID -> Ptr MDNode -> IO ()

foreign import ccall unsafe "LLVM_General_GetMetadata" getMetadata ::
  Ptr Instruction -> Ptr MDKindID -> Ptr (Ptr MDNode) -> CUInt -> IO CUInt
