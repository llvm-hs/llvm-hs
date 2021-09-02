{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ViewPatterns #-}

module LLVM.Internal.FFI.Builder where

import LLVM.Prelude

import Foreign.Ptr
import Foreign.C
import GHC.Stack

import qualified Data.List as List
import qualified Data.Map as Map

import qualified LLVM.AST.Instruction as A
import LLVM.Internal.InstructionDefs as ID

import LLVM.Internal.FFI.Context
import LLVM.Internal.FFI.LLVMCTypes
import LLVM.Internal.FFI.PtrHierarchy

data Builder

foreign import ccall unsafe "LLVMCreateBuilderInContext" createBuilderInContext ::
  Ptr Context -> IO (Ptr Builder)

foreign import ccall unsafe "LLVMDisposeBuilder" disposeBuilder ::
  Ptr Builder -> IO ()

foreign import ccall unsafe "LLVMPositionBuilderAtEnd" positionBuilderAtEnd ::
  Ptr Builder -> Ptr BasicBlock -> IO ()

foreign import ccall unsafe "LLVMBuildRet" buildRet ::
  Ptr Builder -> Ptr Value -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVMBuildBr" buildBr ::
  Ptr Builder -> Ptr BasicBlock -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVMBuildCondBr" buildCondBr ::
  Ptr Builder -> Ptr Value -> Ptr BasicBlock -> Ptr BasicBlock -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVMBuildSwitch" buildSwitch ::
  Ptr Builder -> Ptr Value -> Ptr BasicBlock -> CUInt -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVMBuildIndirectBr" buildIndirectBr ::
  Ptr Builder -> Ptr Value -> CUInt -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVMBuildInvoke" buildInvoke ::
  Ptr Builder -> Ptr Value -> Ptr (Ptr Value) -> CUInt
              -> Ptr BasicBlock -> Ptr BasicBlock -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVMBuildResume" buildResume ::
  Ptr Builder -> Ptr Value -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVMBuildUnreachable" buildUnreachable ::
  Ptr Builder -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_Hs_BuildCleanupRet" buildCleanupRet ::
  Ptr Builder -> Ptr Value -> Ptr BasicBlock -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_Hs_BuildCatchRet" buildCatchRet ::
  Ptr Builder -> Ptr Value -> Ptr BasicBlock -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_Hs_BuildCatchSwitch" buildCatchSwitch ::
  Ptr Builder -> Ptr Value -> Ptr BasicBlock -> CUInt -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_Hs_BuildAdd" buildAdd ::
  Ptr Builder -> LLVMBool -> LLVMBool -> Ptr Value -> Ptr Value -> CString -> IO (Ptr BinaryOperator)

foreign import ccall unsafe "LLVM_Hs_BuildMul" buildMul ::
  Ptr Builder -> LLVMBool -> LLVMBool -> Ptr Value -> Ptr Value -> CString -> IO (Ptr BinaryOperator)

foreign import ccall unsafe "LLVM_Hs_BuildShl" buildShl ::
  Ptr Builder -> LLVMBool -> LLVMBool -> Ptr Value -> Ptr Value -> CString -> IO (Ptr BinaryOperator)

foreign import ccall unsafe "LLVM_Hs_BuildSub" buildSub ::
  Ptr Builder -> LLVMBool -> LLVMBool -> Ptr Value -> Ptr Value -> CString -> IO (Ptr BinaryOperator)

foreign import ccall unsafe "LLVM_Hs_BuildUDiv" buildUDiv ::
  Ptr Builder -> LLVMBool -> Ptr Value -> Ptr Value -> CString -> IO (Ptr BinaryOperator)

foreign import ccall unsafe "LLVM_Hs_BuildSDiv" buildSDiv ::
  Ptr Builder -> LLVMBool -> Ptr Value -> Ptr Value -> CString -> IO (Ptr BinaryOperator)

foreign import ccall unsafe "LLVM_Hs_BuildLShr" buildLShr ::
  Ptr Builder -> LLVMBool -> Ptr Value -> Ptr Value -> CString -> IO (Ptr BinaryOperator)

foreign import ccall unsafe "LLVM_Hs_BuildAShr" buildAShr ::
  Ptr Builder -> LLVMBool -> Ptr Value -> Ptr Value -> CString -> IO (Ptr BinaryOperator)

foreign import ccall unsafe "LLVM_Hs_BuildAddrSpaceCast" buildAddrSpaceCast ::
  Ptr Builder -> Ptr Value -> Ptr Type -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_Hs_BuildBitCast" buildBitCast ::
  Ptr Builder -> Ptr Value -> Ptr Type -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_Hs_BuildFPExt" buildFPExt ::
  Ptr Builder -> Ptr Value -> Ptr Type -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_Hs_BuildFPToSI" buildFPToSI ::
  Ptr Builder -> Ptr Value -> Ptr Type -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_Hs_BuildFPToUI" buildFPToUI ::
  Ptr Builder -> Ptr Value -> Ptr Type -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_Hs_BuildFPTrunc" buildFPTrunc ::
  Ptr Builder -> Ptr Value -> Ptr Type -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_Hs_BuildIntToPtr" buildIntToPtr ::
  Ptr Builder -> Ptr Value -> Ptr Type -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_Hs_BuildPtrToInt" buildPtrToInt ::
  Ptr Builder -> Ptr Value -> Ptr Type -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_Hs_BuildSExt" buildSExt ::
  Ptr Builder -> Ptr Value -> Ptr Type -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_Hs_BuildSIToFP" buildSIToFP ::
  Ptr Builder -> Ptr Value -> Ptr Type -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_Hs_BuildTrunc" buildTrunc ::
  Ptr Builder -> Ptr Value -> Ptr Type -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_Hs_BuildUIToFP" buildUIToFP ::
  Ptr Builder -> Ptr Value -> Ptr Type -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_Hs_BuildZExt" buildZExt ::
  Ptr Builder -> Ptr Value -> Ptr Type -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_Hs_BuildAnd" buildAnd ::
  Ptr Builder -> Ptr Value -> Ptr Value -> CString -> IO (Ptr BinaryOperator)

foreign import ccall unsafe "LLVM_Hs_BuildOr" buildOr ::
  Ptr Builder -> Ptr Value -> Ptr Value -> CString -> IO (Ptr BinaryOperator)

foreign import ccall unsafe "LLVM_Hs_BuildSRem" buildSRem ::
  Ptr Builder -> Ptr Value -> Ptr Value -> CString -> IO (Ptr BinaryOperator)

foreign import ccall unsafe "LLVM_Hs_BuildURem" buildURem ::
  Ptr Builder -> Ptr Value -> Ptr Value -> CString -> IO (Ptr BinaryOperator)

foreign import ccall unsafe "LLVM_Hs_BuildXor" buildXor ::
  Ptr Builder -> Ptr Value -> Ptr Value -> CString -> IO (Ptr BinaryOperator)

foreign import ccall unsafe "LLVM_Hs_BuildFNeg" buildFNeg ::
  Ptr Builder -> Ptr Value -> CString -> IO (Ptr UnaryOperator)

foreign import ccall unsafe "LLVM_Hs_BuildFAdd" buildFAdd ::
  Ptr Builder -> Ptr Value -> Ptr Value -> CString -> IO (Ptr BinaryOperator)

foreign import ccall unsafe "LLVM_Hs_BuildFDiv" buildFDiv ::
  Ptr Builder -> Ptr Value -> Ptr Value -> CString -> IO (Ptr BinaryOperator)

foreign import ccall unsafe "LLVM_Hs_BuildFMul" buildFMul ::
  Ptr Builder -> Ptr Value -> Ptr Value -> CString -> IO (Ptr BinaryOperator)

foreign import ccall unsafe "LLVM_Hs_BuildFRem" buildFRem ::
  Ptr Builder -> Ptr Value -> Ptr Value -> CString -> IO (Ptr BinaryOperator)

foreign import ccall unsafe "LLVM_Hs_BuildFSub" buildFSub ::
  Ptr Builder -> Ptr Value -> Ptr Value -> CString -> IO (Ptr BinaryOperator)

foreign import ccall unsafe "LLVMBuildArrayAlloca" buildAlloca ::
  Ptr Builder -> Ptr Type -> Ptr Value -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_Hs_BuildLoad" buildLoad' ::
  Ptr Builder -> LLVMBool -> Ptr Value -> MemoryOrdering -> SynchronizationScope -> CUInt -> CString -> IO (Ptr Instruction)

buildLoad :: Ptr Builder -> LLVMBool -> Ptr Value -> (SynchronizationScope, MemoryOrdering) -> CUInt -> CString -> IO (Ptr Instruction)
buildLoad builder vol a' (ss, mo) al s = buildLoad' builder vol a' mo ss al s

foreign import ccall unsafe "LLVM_Hs_BuildStore" buildStore' ::
  Ptr Builder -> LLVMBool -> Ptr Value -> Ptr Value -> MemoryOrdering -> SynchronizationScope -> CUInt -> CString -> IO (Ptr Instruction)

buildStore :: Ptr Builder -> LLVMBool -> Ptr Value -> Ptr Value -> (SynchronizationScope, MemoryOrdering) -> CUInt -> CString -> IO (Ptr Instruction)
buildStore builder vol a' v' (ss, mo) al s = buildStore' builder vol a' v' mo ss al s

foreign import ccall unsafe "LLVM_Hs_BuildGEP" buildGetElementPtr' ::
  Ptr Builder -> Ptr Value -> Ptr (Ptr Value) -> CUInt -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_Hs_BuildInBoundsGEP" buildInBoundsGetElementPtr' ::
  Ptr Builder -> Ptr Value -> Ptr (Ptr Value) -> CUInt -> CString -> IO (Ptr Instruction)

buildGetElementPtr :: HasCallStack => Ptr Builder -> LLVMBool -> Ptr Value -> (CUInt, Ptr (Ptr Value)) -> CString -> IO (Ptr Instruction)
buildGetElementPtr builder (LLVMBool 1) a (n, is) s = buildInBoundsGetElementPtr' builder a is n s
buildGetElementPtr builder (LLVMBool 0) a (n, is) s = buildGetElementPtr' builder a is n s
buildGetElementPtr _ (LLVMBool i) _ _ _ = error ("LLVMBool should be 0 or 1 but is " <> show i)

foreign import ccall unsafe "LLVM_Hs_BuildFence" buildFence' ::
  Ptr Builder -> MemoryOrdering -> SynchronizationScope -> CString -> IO (Ptr Instruction)

buildFence :: Ptr Builder -> (SynchronizationScope, MemoryOrdering) -> CString -> IO (Ptr Instruction)
buildFence builder (ss, mo) s = buildFence' builder mo ss s

foreign import ccall unsafe "LLVM_Hs_BuildAtomicCmpXchg" buildCmpXchg' ::
  Ptr Builder -> LLVMBool -> Ptr Value -> Ptr Value -> Ptr Value -> MemoryOrdering -> MemoryOrdering -> SynchronizationScope -> CString -> IO (Ptr Instruction)

buildCmpXchg :: Ptr Builder -> LLVMBool -> Ptr Value -> Ptr Value -> Ptr Value -> (SynchronizationScope, MemoryOrdering) -> MemoryOrdering -> CString -> IO (Ptr Instruction)
buildCmpXchg builder vol a e r (ss, smo) fmo s =  buildCmpXchg' builder vol a e r smo fmo ss s

foreign import ccall unsafe "LLVM_Hs_BuildAtomicRMW" buildAtomicRMW' ::
  Ptr Builder -> LLVMBool -> RMWOperation -> Ptr Value -> Ptr Value -> MemoryOrdering -> SynchronizationScope -> CString -> IO (Ptr Instruction)

buildAtomicRMW :: Ptr Builder -> LLVMBool -> RMWOperation -> Ptr Value -> Ptr Value -> (SynchronizationScope, MemoryOrdering) -> CString -> IO (Ptr Instruction)
buildAtomicRMW builder vol rmwOp a v (ss, mo) s = buildAtomicRMW' builder vol rmwOp a v mo ss s

foreign import ccall unsafe "LLVM_Hs_BuildICmp" buildICmp ::
  Ptr Builder -> ICmpPredicate -> Ptr Value -> Ptr Value -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVMBuildFCmp" buildFCmp ::
  Ptr Builder -> FCmpPredicate -> Ptr Value -> Ptr Value -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVMBuildPhi" buildPhi ::
  Ptr Builder -> Ptr Type -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVMBuildCall" buildCall ::
  Ptr Builder -> Ptr Value -> Ptr (Ptr Value) -> CUInt -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVMBuildFreeze" buildFreeze ::
  Ptr Builder -> Ptr Value -> Ptr Type -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_Hs_BuildSelect" buildSelect ::
  Ptr Builder -> Ptr Value -> Ptr Value -> Ptr Value -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVMBuildVAArg" buildVAArg ::
  Ptr Builder -> Ptr Value -> Ptr Type -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_Hs_BuildExtractElement" buildExtractElement ::
  Ptr Builder -> Ptr Value -> Ptr Value -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_Hs_BuildInsertElement" buildInsertElement ::
  Ptr Builder -> Ptr Value -> Ptr Value -> Ptr Value -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_Hs_BuildShuffleVector" buildShuffleVector ::
  Ptr Builder -> Ptr Value -> Ptr Value -> Ptr CInt -> CUInt -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_Hs_BuildExtractValue" buildExtractValue ::
  Ptr Builder -> Ptr Value -> Ptr CUInt -> CUInt -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_Hs_BuildInsertValue" buildInsertValue ::
  Ptr Builder -> Ptr Value -> Ptr Value -> Ptr CUInt -> CUInt -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVMBuildLandingPad" buildLandingPad' ::
  Ptr Builder -> Ptr Type -> Ptr Value -> CUInt -> CString -> IO (Ptr Instruction)

-- | The personality should be set via the function
buildLandingPad :: Ptr Builder -> Ptr Type -> CUInt -> CString -> IO (Ptr Instruction)
buildLandingPad builder ty numClauses name = buildLandingPad' builder ty nullPtr numClauses name

foreign import ccall unsafe "LLVM_Hs_BuildCleanupPad" buildCleanupPad ::
  Ptr Builder -> Ptr Value -> Ptr (Ptr Value) -> CUInt -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_Hs_BuildCatchPad" buildCatchPad ::
  Ptr Builder -> Ptr Value -> Ptr (Ptr Value) -> CUInt -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_Hs_SetFastMathFlags" setFastMathFlags ::
  Ptr Builder -> FastMathFlags -> IO ()
