{-# LANGUAGE
  ForeignFunctionInterface,
  MultiParamTypeClasses,
  UndecidableInstances,
  TemplateHaskell
  #-}
module LLVM.Internal.FFI.Instruction where

import LLVM.Prelude

import Foreign.Ptr
import Foreign.C

import LLVM.Internal.FFI.Attribute
import LLVM.Internal.FFI.PtrHierarchy
import LLVM.Internal.FFI.LLVMCTypes

foreign import ccall unsafe "LLVMIsAInstruction" isAInstruction ::
  Ptr Value -> IO (Ptr Instruction)

newtype COpcode = COpcode CUInt

-- get the C API opcode
foreign import ccall unsafe "LLVMGetInstructionOpcode" getInstructionOpcode :: 
  Ptr Instruction -> IO COpcode

-- get the C++ API opcode (one less level of mapping than for that from the C API)
foreign import ccall unsafe "LLVM_Hs_GetInstructionDefOpcode" getInstructionDefOpcode :: 
  Ptr Instruction -> IO CPPOpcode

foreign import ccall unsafe "LLVMGetICmpPredicate" getICmpPredicate ::
  Ptr Instruction -> IO ICmpPredicate

foreign import ccall unsafe "LLVM_Hs_GetFCmpPredicate" getFCmpPredicate ::
  Ptr Instruction -> IO FCmpPredicate

foreign import ccall unsafe "LLVM_Hs_GetCallSiteCallingConvention" getCallSiteCallingConvention ::
  Ptr Instruction -> IO CallingConvention

foreign import ccall unsafe "LLVM_Hs_SetCallSiteCallingConvention" setCallSiteCallingConvention ::
  Ptr Instruction -> CallingConvention -> IO ()

foreign import ccall unsafe "LLVM_Hs_GetTailCallKind" getTailCallKind ::
  Ptr Instruction -> IO TailCallKind

foreign import ccall unsafe "LLVM_Hs_SetTailCallKind" setTailCallKind ::
  Ptr Instruction -> TailCallKind -> IO ()

foreign import ccall unsafe "LLVMGetCalledValue" getCallSiteCalledValue ::
  Ptr Instruction -> IO (Ptr Value)

foreign import ccall unsafe "LLVMGetNumArgOperands" getCallSiteNumArgOperands ::
  Ptr Instruction -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_CallSiteAttributesAtIndex" getCallSiteAttributesAtIndex ::
  Ptr Instruction -> AttributeIndex -> IO (AttributeSet a)

foreign import ccall unsafe "LLVM_Hs_CallSiteSetAttributeList" setCallSiteAttributeList ::
  Ptr Instruction -> AttributeList -> IO ()
                     
foreign import ccall unsafe "LLVMAddIncoming" addIncoming' ::
  Ptr Instruction -> Ptr (Ptr Value) -> Ptr (Ptr BasicBlock) -> CUInt -> IO ()

addIncoming :: Ptr Instruction -> (CUInt, Ptr (Ptr Value)) -> (CUInt, Ptr (Ptr BasicBlock)) -> IO ()
addIncoming i (nvs, vs) (nbs, bs)
  | nbs == nvs = addIncoming' i vs bs nbs
  | otherwise = error "Number of incoming values and incoming blocks must be equal"

foreign import ccall unsafe "LLVMCountIncoming" countIncoming ::
  Ptr Instruction -> IO CUInt

foreign import ccall unsafe "LLVMGetIncomingValue" getIncomingValue ::
  Ptr Instruction -> CUInt -> IO (Ptr Value)

foreign import ccall unsafe "LLVMGetIncomingBlock" getIncomingBlock ::
  Ptr Instruction -> CUInt -> IO (Ptr BasicBlock)


foreign import ccall unsafe "LLVMAddCase" addCase ::
  Ptr Instruction -> Ptr Constant -> Ptr BasicBlock -> IO ()

foreign import ccall unsafe "LLVM_Hs_GetSwitchCases" getSwitchCases ::
  Ptr Instruction -> Ptr (Ptr Constant) -> Ptr (Ptr BasicBlock) -> IO ()

foreign import ccall unsafe "LLVMAddDestination" addDestination ::
  Ptr Instruction -> Ptr BasicBlock -> IO ()

foreign import ccall unsafe "LLVM_Hs_GetIndirectBrDests" getIndirectBrDests ::
  Ptr Instruction -> Ptr (Ptr BasicBlock) -> IO ()


foreign import ccall unsafe "LLVM_Hs_GetInstrAlignment" getInstrAlignment ::
  Ptr Instruction -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_SetInstrAlignment" setInstrAlignment ::
  Ptr Instruction -> CUInt -> IO ()

foreign import ccall unsafe "LLVM_Hs_GetAllocaNumElements" getAllocaNumElements ::
  Ptr Instruction -> IO (Ptr Value)

foreign import ccall unsafe "LLVM_Hs_GetAllocatedType" getAllocatedType ::
  Ptr Instruction -> IO (Ptr Type)

foreign import ccall unsafe "LLVM_Hs_GetAtomicOrdering" getAtomicOrdering ::
  Ptr Instruction -> IO MemoryOrdering

foreign import ccall unsafe "LLVM_Hs_GetFailureAtomicOrdering" getFailureAtomicOrdering ::
  Ptr Instruction -> IO MemoryOrdering

foreign import ccall unsafe "LLVM_Hs_GetSynchronizationScope" getSynchronizationScope ::
  Ptr Instruction -> IO SynchronizationScope

getAtomicity :: Ptr Instruction -> IO (SynchronizationScope, MemoryOrdering)
getAtomicity i = return (,) `ap` getSynchronizationScope i `ap` getAtomicOrdering i

foreign import ccall unsafe "LLVM_Hs_GetVolatile" getVolatile ::
  Ptr Instruction -> IO LLVMBool

foreign import ccall unsafe "LLVM_Hs_GetInBounds" getInBounds ::
  Ptr Value -> IO LLVMBool

foreign import ccall unsafe "LLVM_Hs_GetAtomicRMWBinOp" getAtomicRMWBinOp ::
  Ptr Instruction -> IO RMWOperation

foreign import ccall unsafe "LLVM_Hs_CountInstStructureIndices" countInstStructureIndices ::
  Ptr Instruction -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_GetInstStructureIndices" getInstStructureIndices ::
  Ptr Instruction -> Ptr CUInt -> IO ()

foreign import ccall unsafe "LLVMAddClause" addClause ::
  Ptr Instruction -> Ptr Constant -> IO ()

foreign import ccall unsafe "LLVMSetCleanup" setCleanup ::
  Ptr Instruction -> LLVMBool -> IO ()

foreign import ccall unsafe "LLVMIsCleanup" isCleanup ::
  Ptr Instruction -> IO LLVMBool

foreign import ccall unsafe "LLVMGetNumClauses" getNumClauses ::
  Ptr Instruction -> IO CUInt

foreign import ccall unsafe "LLVMGetClause" getClause ::
  Ptr Instruction -> CUInt -> IO (Ptr Constant)

foreign import ccall unsafe "LLVM_Hs_SetMetadata" setMetadata ::
  Ptr Instruction -> MDKindID -> Ptr MDNode -> IO ()

foreign import ccall unsafe "LLVM_Hs_GetMetadata" getMetadata ::
  Ptr Instruction -> Ptr MDKindID -> Ptr (Ptr MDNode) -> CUInt -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_GetCleanupPad" getCleanupPad ::
  Ptr Instruction -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_Hs_GetUnwindDest" getUnwindDest ::
  Ptr Instruction -> IO (Ptr BasicBlock)

foreign import ccall unsafe "LLVM_Hs_GetParentPad" getParentPad ::
  Ptr Instruction -> IO (Ptr Value)

foreign import ccall unsafe "LLVM_Hs_GetNumArgOperands" getNumArgOperands ::
  Ptr Instruction -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_GetArgOperand" getArgOperand ::
  Ptr Instruction -> CUInt -> IO (Ptr Value)

foreign import ccall unsafe "LLVM_Hs_CatchSwitch_GetParentPad" catchSwitchGetParentPad ::
  Ptr Instruction -> IO (Ptr Value)

foreign import ccall unsafe "LLVM_Hs_CatchSwitch_GetUnwindDest" catchSwitchGetUnwindDest ::
  Ptr Instruction -> IO (Ptr BasicBlock)

foreign import ccall unsafe "LLVM_Hs_CatchSwitch_GetNumHandlers" catchSwitchGetNumHandlers ::
  Ptr Instruction -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_CatchSwitch_GetHandler" catchSwitchGetHandler ::
  Ptr Instruction -> CUInt -> IO (Ptr BasicBlock)

foreign import ccall unsafe "LLVM_Hs_CatchSwitch_AddHandler" catchSwitchAddHandler ::
  Ptr Instruction -> Ptr BasicBlock -> IO ()

foreign import ccall unsafe "LLVM_Hs_CatchRet_GetCatchPad" catchRetGetCatchPad ::
  Ptr Instruction -> IO (Ptr Value)

foreign import ccall unsafe "LLVM_Hs_CatchRet_GetSuccessor" catchRetGetSuccessor ::
  Ptr Instruction -> IO (Ptr BasicBlock)
