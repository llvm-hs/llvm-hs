{-#
  LANGUAGE
  ForeignFunctionInterface,
  TemplateHaskell,
  ViewPatterns
  #-}
-- | FFI glue for llvm::IRBuilder - llvm's IR construction state object
module LLVM.General.Internal.FFI.Builder where

import qualified Language.Haskell.TH as TH

import Control.Monad

import qualified LLVM.General.AST.Instruction as A

import LLVM.General.Internal.InstructionDefs as ID

import Foreign.Ptr
import Foreign.C

import qualified Data.Map as Map

import LLVM.General.Internal.FFI.Cleanup
import LLVM.General.Internal.FFI.Context
import LLVM.General.Internal.FFI.LLVMCTypes
import LLVM.General.Internal.FFI.PtrHierarchy

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


$(do
  liftM concat $ sequence $ do
    let instrInfo = ID.outerJoin ID.astInstructionRecs ID.instructionDefs
    (lrn, ii) <- Map.toList instrInfo
    (TH.RecC _ (unzip3 -> (_, _, fieldTypes)), ID.InstructionDef { ID.cAPIName = a, ID.instructionKind = k }) <- case ii of
      (Just r, Just d) -> return (r,d)
      (Just _, Nothing) -> error $ "An AST instruction was not found in the LLVM instruction defs"
      (Nothing, Just ID.InstructionDef { ID.instructionKind = k }) | k /= ID.Terminator -> 
        error $ "LLVM instruction def " ++ lrn ++ " not found in the AST"
      _ -> []

    let ats = map typeMapping [ t | t <- fieldTypes, t /= TH.ConT ''A.InstructionMetadata ]
        cName = (if hasFlags fieldTypes then "LLVM_General_" else "LLVM") ++ "Build" ++ a
    rt <- case k of
            ID.Binary -> [[t| BinaryOperator |]]
            ID.Cast -> [[t| Instruction |]]
            _ -> []
    return $ foreignDecl cName ("build" ++ a) ([[t| Ptr Builder |]] ++ ats ++ [[t| CString |]]) [t| Ptr $(rt) |]
 )

foreign import ccall unsafe "LLVMBuildArrayAlloca" buildAlloca ::
  Ptr Builder -> Ptr Type -> Ptr Value -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_General_BuildLoad" buildLoad ::
  Ptr Builder -> Ptr Value -> CUInt -> LLVMBool -> MemoryOrdering -> LLVMBool -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_General_BuildStore" buildStore ::
  Ptr Builder -> Ptr Value -> Ptr Value -> CUInt -> LLVMBool -> MemoryOrdering -> LLVMBool -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVMBuildGEP" buildGetElementPtr' ::
  Ptr Builder -> Ptr Value -> Ptr (Ptr Value) -> CUInt -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVMBuildInBoundsGEP" buildInBoundsGetElementPtr' ::
  Ptr Builder -> Ptr Value -> Ptr (Ptr Value) -> CUInt -> CString -> IO (Ptr Instruction)

buildGetElementPtr :: Ptr Builder -> LLVMBool -> Ptr Value -> Ptr (Ptr Value) -> CUInt -> CString -> IO (Ptr Instruction)
buildGetElementPtr builder (LLVMBool 1) = buildInBoundsGetElementPtr' builder
buildGetElementPtr builder (LLVMBool 0) = buildGetElementPtr' builder

foreign import ccall unsafe "LLVMBuildFence" buildFence ::
  Ptr Builder -> MemoryOrdering -> LLVMBool -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_General_BuildAtomicCmpXchg" buildCmpXchg ::
  Ptr Builder -> Ptr Value -> Ptr Value -> Ptr Value -> LLVMBool -> MemoryOrdering -> LLVMBool -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_General_BuildAtomicRMW" buildAtomicRMW ::
  Ptr Builder -> RMWOperation -> Ptr Value -> Ptr Value -> LLVMBool -> MemoryOrdering -> LLVMBool -> CString -> IO (Ptr Instruction)


foreign import ccall unsafe "LLVMBuildICmp" buildICmp ::
  Ptr Builder -> ICmpPredicate -> Ptr Value -> Ptr Value -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVMBuildFCmp" buildFCmp ::
  Ptr Builder -> FCmpPredicate -> Ptr Value -> Ptr Value -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVMBuildPhi" buildPhi ::
  Ptr Builder -> Ptr Type -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVMBuildCall" buildCall ::
  Ptr Builder -> Ptr Value -> Ptr (Ptr Value) -> CUInt -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVMBuildSelect" buildSelect ::
  Ptr Builder -> Ptr Value -> Ptr Value -> Ptr Value -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVMBuildVAArg" buildVAArg ::
  Ptr Builder -> Ptr Value -> Ptr Type -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVMBuildExtractElement" buildExtractElement ::
  Ptr Builder -> Ptr Value -> Ptr Value -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVMBuildInsertElement" buildInsertElement ::
  Ptr Builder -> Ptr Value -> Ptr Value -> Ptr Value -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVMBuildShuffleVector" buildShuffleVector ::
  Ptr Builder -> Ptr Value -> Ptr Value -> Ptr Constant -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_General_BuildExtractValue" buildExtractValue ::
  Ptr Builder -> Ptr Value -> Ptr CUInt -> CUInt -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVM_General_BuildInsertValue" buildInsertValue ::
  Ptr Builder -> Ptr Value -> Ptr Value -> Ptr CUInt -> CUInt -> CString -> IO (Ptr Instruction)

foreign import ccall unsafe "LLVMBuildLandingPad" buildLandingPad ::
  Ptr Builder -> Ptr Type -> Ptr Value -> CUInt -> CString -> IO (Ptr Instruction)


