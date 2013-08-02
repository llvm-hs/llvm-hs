{-# LANGUAGE
  TemplateHaskell,
  MultiParamTypeClasses
  #-}
module LLVM.General.Internal.IntegerPredicate where

import LLVM.General.Internal.Coding

import qualified LLVM.General.Internal.FFI.LLVMCTypes as FFI
import qualified LLVM.General.AST.IntegerPredicate as A.IPred

genCodingInstance [t| A.IPred.IntegerPredicate |] ''FFI.ICmpPredicate [
  (FFI.iCmpPredEQ, A.IPred.EQ),
  (FFI.iCmpPredNE, A.IPred.NE),
  (FFI.iCmpPredUGT, A.IPred.UGT),
  (FFI.iCmpPredUGE, A.IPred.UGE),
  (FFI.iCmpPredULT, A.IPred.ULT),
  (FFI.iCmpPredULE, A.IPred.ULE),
  (FFI.iCmpPredSGT, A.IPred.SGT),
  (FFI.iCmpPredSGE, A.IPred.SGE),
  (FFI.iCmpPredSLT, A.IPred.SLT),
  (FFI.iCmpPredSLE, A.IPred.SLE)
 ]
