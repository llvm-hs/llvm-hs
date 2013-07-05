{-#
  LANGUAGE
  ForeignFunctionInterface,
  TemplateHaskell
  #-}
-- | FFI glue for llvm::IRBuilder - llvm's IR construction state object
module LLVM.General.Internal.FFI.Builder where

import qualified Language.Haskell.TH as TH

import qualified LLVM.General.AST.Instruction as A
import qualified LLVM.General.AST.Operand as A
import qualified LLVM.General.AST.Type as A

import LLVM.General.Internal.InstructionDefs as ID

import Foreign.Ptr
import Foreign.C

import qualified Data.Map as Map

import LLVM.General.Internal.FFI.LLVMCTypes
import LLVM.General.Internal.FFI.Context
import LLVM.General.Internal.FFI.BinaryOperator
import LLVM.General.Internal.FFI.Type
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
  sequence [
     TH.forImpD TH.cCall TH.unsafe cName (TH.mkName ("build" ++ a)) [t| 
       Ptr Builder -> $(foldr (\at rt -> [t| $(at) -> $(rt) |]) [t| CString -> IO (Ptr $(rt)) |] ats)
       |]
     | (lrn, ID.InstructionDef { ID.cAPIName = a, ID.instructionKind = k }) <- Map.toList ID.instructionDefs,
       let fieldTypes = 
             (\(TH.RecC _ fs) -> [ t | (_,_,t) <- fs]) $
             Map.findWithDefault 
                  (error $ "LLVM instruction not found in AST: " ++ show lrn)
                  lrn ID.astInstructionRecs
           ats = flip concatMap fieldTypes $ \ft ->
                 case ft of
                   TH.ConT h | h == ''Bool -> [[t| LLVMBool |]]
                             | h == ''A.Operand -> [[t| Ptr Value |]]
                             | h == ''A.Type -> [[t| Ptr Type |]]
                             | h == ''A.InstructionMetadata -> []
                   _ -> error $ "show ft = " ++ show ft
           cName = (if TH.ConT ''Bool `elem` fieldTypes then "LLVM_General_" else "LLVM") ++ "Build" ++ a,
       rt <- case k of
               ID.Binary -> [[t| BinaryOperator |]]
               ID.Cast -> [[t| Instruction |]]
               _ -> []
    ]
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


