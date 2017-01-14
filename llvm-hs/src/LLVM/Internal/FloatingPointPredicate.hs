{-# LANGUAGE
  MultiParamTypeClasses,
  TemplateHaskell
  #-}
module LLVM.Internal.FloatingPointPredicate where

import LLVM.Internal.Coding

import qualified LLVM.Internal.FFI.LLVMCTypes as FFI
import qualified LLVM.AST.FloatingPointPredicate as A.FPPred

genCodingInstance [t| A.FPPred.FloatingPointPredicate |] ''FFI.FCmpPredicate [
  (FFI.fCmpPredFalse, A.FPPred.False),
  (FFI.fCmpPredOEQ, A.FPPred.OEQ),
  (FFI.fCmpPredOGT, A.FPPred.OGT),
  (FFI.fCmpPredOGE, A.FPPred.OGE),
  (FFI.fCmpPredOLT, A.FPPred.OLT),
  (FFI.fCmpPredOLE, A.FPPred.OLE),
  (FFI.fCmpPredONE, A.FPPred.ONE),
  (FFI.fCmpPredORD, A.FPPred.ORD),
  (FFI.fCmpPredUNO, A.FPPred.UNO),
  (FFI.fCmpPredUEQ, A.FPPred.UEQ),
  (FFI.fCmpPredUGT, A.FPPred.UGT),
  (FFI.fCmpPredUGE, A.FPPred.UGE),
  (FFI.fCmpPredULT, A.FPPred.ULT),
  (FFI.fCmpPredULE, A.FPPred.ULE),
  (FFI.fCmpPredUNE, A.FPPred.UNE),
  (FFI.fcmpPredTrue, A.FPPred.True)
 ]
