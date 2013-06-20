{-# LANGUAGE
  TemplateHaskell,
  ForeignFunctionInterface,
  EmptyDataDecls,
  MultiParamTypeClasses,
  FlexibleInstances,
  UndecidableInstances,
  OverlappingInstances
  #-}

-- | FFI functions for handling the LLVM Constant class
module LLVM.General.Internal.FFI.Constant where

import qualified Language.Haskell.TH as TH
import qualified LLVM.General.Internal.InstructionDefs as ID
import qualified LLVM.General.AST.Constant as A.C

import Control.Monad
import Data.Function
import qualified Data.Map as Map

import Foreign.Ptr
import Foreign.C

import LLVM.General.Internal.FFI.PtrHierarchy
import LLVM.General.Internal.FFI.Context
import LLVM.General.Internal.FFI.Cleanup
import LLVM.General.Internal.FFI.Type
import LLVM.General.Internal.FFI.LLVMCTypes

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

foreign import ccall unsafe "LLVM_General_GetConstantIntWords" getConstantIntWords ::
  Ptr Constant -> Ptr CUInt -> IO (Ptr CULong)

foreign import ccall unsafe "LLVM_General_ConstFloatDoubleValue" constFloatDoubleValue ::
  Ptr Constant -> IO CDouble

foreign import ccall unsafe "LLVM_General_ConstFloatFloatValue" constFloatFloatValue ::
  Ptr Constant -> IO CFloat

foreign import ccall unsafe "LLVMConstStructInContext" constStructInContext' ::
  Ptr Context -> Ptr (Ptr Constant) -> CUInt -> LLVMBool -> IO (Ptr Constant)

constStructInContext ctx (n, cs) p = constStructInContext' ctx cs n p

foreign import ccall unsafe "LLVM_General_GetConstantDataSequentialElementAsConstant" getConstantDataSequentialElementAsConstant ::
  Ptr Constant -> CUInt -> IO (Ptr Constant)

foreign import ccall unsafe "LLVMConstIntOfArbitraryPrecision" constantIntOfArbitraryPrecision' ::
  Ptr Type -> CUInt -> Ptr CULong -> IO (Ptr Constant)

constantIntOfArbitraryPrecision t = uncurry (constantIntOfArbitraryPrecision' t)

foreign import ccall unsafe "LLVM_General_ConstFloatOfArbitraryPrecision" constantFloatOfArbitraryPrecision ::
  Ptr Context -> CUInt -> Ptr CULong -> LLVMBool -> IO (Ptr Constant)

foreign import ccall unsafe "LLVM_General_GetConstantFloatWords" getConstantFloatWords ::
  Ptr Constant -> Ptr CULong -> IO ()

foreign import ccall unsafe "LLVMConstVector" constantVector' ::
  Ptr (Ptr Constant) -> CUInt -> IO (Ptr Constant)

constantVector (n, cs) = constantVector' cs n

foreign import ccall unsafe "LLVMConstNull" constantNull ::
  Ptr Type -> IO (Ptr Constant)

foreign import ccall unsafe "LLVMConstArray" constantArray' ::
  Ptr Type -> Ptr (Ptr Constant) -> CUInt -> IO (Ptr Constant)

constantArray t (n, cs) = constantArray' t cs n

foreign import ccall unsafe "LLVM_General_ConstCast" constantCast ::
  CPPOpcode -> Ptr Constant -> Ptr Type -> IO (Ptr Constant)

foreign import ccall unsafe "LLVM_General_ConstBinaryOperator" constantBinaryOperator ::
  CPPOpcode -> Ptr Constant -> Ptr Constant -> IO (Ptr Constant)

$(do
   let constExprInfo = ID.innerJoin (ID.innerJoin ID.astConstantRecs ID.astInstructionRecs) ID.instructionDefs
   let tm = fix tm' 
         where tm' _ (TH.ConT h) | h == ''A.C.Constant = [t| Ptr Constant |]
               tm' x t = typeMappingU x t
               
   liftM concat $ sequence [      
     foreignDecl ("LLVMConst" ++ name) ("constant" ++ name) [tm t | (_, _, t) <- fs ] [t| Ptr Constant |]
     | (name, ((TH.RecC _ fs,_), ID.InstructionDef { ID.instructionKind = ID.Other })) <- Map.toList constExprInfo
    ]
  )

foreign import ccall unsafe "LLVMConstGEP" constantGetElementPtr' ::
  Ptr Constant -> Ptr (Ptr Constant) -> CUInt -> IO (Ptr Constant)

foreign import ccall unsafe "LLVMConstInBoundsGEP" constantInBoundsGetElementPtr' ::
  Ptr Constant -> Ptr (Ptr Constant) -> CUInt -> IO (Ptr Constant)

constantGetElementPtr (LLVMBool ib) a (n, is) = 
  (case ib of { 0 -> constantGetElementPtr'; 1 -> constantInBoundsGetElementPtr' }) a is n

foreign import ccall unsafe "LLVM_General_GetConstCPPOpcode" getConstantCPPOpcode ::
  Ptr Constant -> IO CPPOpcode

foreign import ccall unsafe "LLVM_General_GetConstPredicate" getConstantICmpPredicate ::
  Ptr Constant -> IO ICmpPredicate

foreign import ccall unsafe "LLVM_General_GetConstPredicate" getConstantFCmpPredicate ::
  Ptr Constant -> IO FCmpPredicate

foreign import ccall unsafe "LLVM_General_GetConstIndices" getConstantIndices ::
  Ptr Constant -> Ptr CUInt -> IO (Ptr CUInt)

foreign import ccall unsafe "LLVMGetUndef" getUndef ::
  Ptr Type -> IO (Ptr Constant)

foreign import ccall unsafe "LLVMBlockAddress" blockAddress ::
  Ptr Value -> Ptr BasicBlock -> IO (Ptr Constant)

foreign import ccall unsafe "LLVM_General_GetBlockAddressFunction" getBlockAddressFunction ::
  Ptr Constant -> IO (Ptr Value)

foreign import ccall unsafe "LLVM_General_GetBlockAddressBlock" getBlockAddressBlock ::
  Ptr Constant -> IO (Ptr BasicBlock)
