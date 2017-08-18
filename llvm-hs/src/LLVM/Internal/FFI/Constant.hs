{-# LANGUAGE
  TemplateHaskell,
  ForeignFunctionInterface,
  MultiParamTypeClasses,
  UndecidableInstances,
  ViewPatterns
  #-}
-- | FFI functions for handling the LLVM Constant class
module LLVM.Internal.FFI.Constant where

import LLVM.Prelude

import qualified Language.Haskell.TH as TH
import qualified LLVM.Internal.InstructionDefs as ID

import qualified Data.Map as Map

import Foreign.Ptr
import Foreign.C

import LLVM.Internal.FFI.PtrHierarchy
import LLVM.Internal.FFI.Context
import LLVM.Internal.FFI.Cleanup
import LLVM.Internal.FFI.LLVMCTypes

foreign import ccall unsafe "LLVMIsConstant" isConstant ::
  Ptr Value -> IO (CUInt)

foreign import ccall unsafe "LLVMIsAConstant" isAConstant ::
  Ptr Value -> IO (Ptr Constant)

foreign import ccall unsafe "LLVMIsAConstantInt" isAConstantInt ::
  Ptr Value -> IO (Ptr Constant)

foreign import ccall unsafe "LLVMGetOperand" getConstantOperand ::
  Ptr Constant -> CUInt -> IO (Ptr Constant)

foreign import ccall unsafe "LLVMIsAConstantPointerNull" isAConstantPointerNull ::
  Ptr Value -> IO (Ptr Constant)

foreign import ccall unsafe "LLVM_Hs_GetConstantIntWords" getConstantIntWords ::
  Ptr Constant -> Ptr CUInt -> IO (Ptr Word64)

foreign import ccall unsafe "LLVM_Hs_ConstFloatDoubleValue" constFloatDoubleValue ::
  Ptr Constant -> IO CDouble

foreign import ccall unsafe "LLVM_Hs_ConstFloatFloatValue" constFloatFloatValue ::
  Ptr Constant -> IO CFloat

foreign import ccall unsafe "LLVMConstStructInContext" constStructInContext' ::
  Ptr Context -> Ptr (Ptr Constant) -> CUInt -> LLVMBool -> IO (Ptr Constant)

constStructInContext :: Ptr Context -> (CUInt, Ptr (Ptr Constant)) -> LLVMBool -> IO (Ptr Constant)
constStructInContext ctx (n, cs) p = constStructInContext' ctx cs n p

foreign import ccall unsafe "LLVMConstNamedStruct" constNamedStruct' ::
  Ptr Type -> Ptr (Ptr Constant) -> CUInt -> IO (Ptr Constant)

constNamedStruct :: Ptr Type -> (CUInt, Ptr (Ptr Constant)) -> IO (Ptr Constant)
constNamedStruct ty (n, cs) = constNamedStruct' ty cs n

foreign import ccall unsafe "LLVM_Hs_GetConstantDataSequentialElementAsConstant" getConstantDataSequentialElementAsConstant ::
  Ptr Constant -> CUInt -> IO (Ptr Constant)

foreign import ccall unsafe "LLVMConstIntOfArbitraryPrecision" constantIntOfArbitraryPrecision' ::
  Ptr Type -> CUInt -> Ptr Word64 -> IO (Ptr Constant)

constantIntOfArbitraryPrecision :: Ptr Type -> (CUInt, Ptr Word64) -> IO (Ptr Constant)
constantIntOfArbitraryPrecision t = uncurry (constantIntOfArbitraryPrecision' t)

foreign import ccall unsafe "LLVM_Hs_ConstFloatOfArbitraryPrecision" constantFloatOfArbitraryPrecision ::
  Ptr Context -> CUInt -> Ptr Word64 -> FloatSemantics -> IO (Ptr Constant)

foreign import ccall unsafe "LLVM_Hs_GetConstantFloatWords" getConstantFloatWords ::
  Ptr Constant -> Ptr Word64 -> IO ()

foreign import ccall unsafe "LLVMConstVector" constantVector' ::
  Ptr (Ptr Constant) -> CUInt -> IO (Ptr Constant)

constantVector :: (CUInt, Ptr (Ptr Constant)) -> IO (Ptr Constant)
constantVector (n, cs) = constantVector' cs n

foreign import ccall unsafe "LLVMConstNull" constantNull ::
  Ptr Type -> IO (Ptr Constant)

foreign import ccall unsafe "LLVMConstArray" constantArray' ::
  Ptr Type -> Ptr (Ptr Constant) -> CUInt -> IO (Ptr Constant)

constantArray :: Ptr Type -> (CUInt, Ptr (Ptr Constant)) -> IO (Ptr Constant)
constantArray t (n, cs) = constantArray' t cs n

foreign import ccall unsafe "LLVM_Hs_ConstCast" constantCast ::
  CPPOpcode -> Ptr Constant -> Ptr Type -> IO (Ptr Constant)

foreign import ccall unsafe "LLVM_Hs_ConstBinaryOperator" constantBinaryOperator ::
  CPPOpcode -> Ptr Constant -> Ptr Constant -> IO (Ptr Constant)

$(do
   let constExprInfo = ID.innerJoin (ID.innerJoin ID.astConstantRecs ID.astInstructionRecs) ID.instructionDefs
   liftM concat $ sequence $ do
     (name, ((TH.RecC _ (unzip3 -> (_, _, fieldTypes)),_), ID.InstructionDef { ID.instructionKind = ik })) <- Map.toList constExprInfo
     prefix <- case ik of
                 ID.Other -> return "LLVM"
                 ID.Binary | hasFlags fieldTypes -> return "LLVM_Hs_"
                 _ -> []
     return $
       foreignDecl (prefix ++ "Const" ++ name) ("constant" ++ name) (map typeMapping fieldTypes) [t| Ptr Constant |]
  )

foreign import ccall unsafe "LLVMConstGEP" constantGetElementPtr' ::
  Ptr Constant -> Ptr (Ptr Constant) -> CUInt -> IO (Ptr Constant)

foreign import ccall unsafe "LLVMConstInBoundsGEP" constantInBoundsGetElementPtr' ::
  Ptr Constant -> Ptr (Ptr Constant) -> CUInt -> IO (Ptr Constant)

constantGetElementPtr :: LLVMBool -> Ptr Constant -> (CUInt, Ptr (Ptr Constant)) -> IO (Ptr Constant)
constantGetElementPtr (LLVMBool ib) a (n, is) =
  (case ib of
     0 -> constantGetElementPtr'
     1 -> constantInBoundsGetElementPtr'
     _ -> error ("LLVMBool should be 0 or 1 but is " <> show ib)
  ) a is n

foreign import ccall unsafe "LLVM_Hs_GetConstCPPOpcode" getConstantCPPOpcode ::
  Ptr Constant -> IO CPPOpcode

foreign import ccall unsafe "LLVM_Hs_GetConstPredicate" getConstantICmpPredicate ::
  Ptr Constant -> IO ICmpPredicate

foreign import ccall unsafe "LLVM_Hs_GetConstPredicate" getConstantFCmpPredicate ::
  Ptr Constant -> IO FCmpPredicate

foreign import ccall unsafe "LLVM_Hs_GetConstIndices" getConstantIndices ::
  Ptr Constant -> Ptr CUInt -> IO (Ptr CUInt)

foreign import ccall unsafe "LLVMGetUndef" constantUndef ::
  Ptr Type -> IO (Ptr Constant)

foreign import ccall unsafe "LLVMBlockAddress" blockAddress ::
  Ptr Value -> Ptr BasicBlock -> IO (Ptr Constant)

foreign import ccall unsafe "LLVM_Hs_GetBlockAddressFunction" getBlockAddressFunction ::
  Ptr Constant -> IO (Ptr Value)

foreign import ccall unsafe "LLVM_Hs_GetBlockAddressBlock" getBlockAddressBlock ::
  Ptr Constant -> IO (Ptr BasicBlock)

foreign import ccall unsafe "LLVM_Hs_GetConstTokenNone" getConstTokenNone ::
  Ptr Context -> IO (Ptr Constant)
