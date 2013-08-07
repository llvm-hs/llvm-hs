{-# LANGUAGE
  TemplateHaskell,
  MultiParamTypeClasses
  #-}
module LLVM.General.Internal.RMWOperation where

import LLVM.General.AST.RMWOperation

import qualified LLVM.General.Internal.FFI.LLVMCTypes as FFI

import LLVM.General.Internal.Coding

genCodingInstance [t| RMWOperation |] ''FFI.RMWOperation [
  (FFI.rmwOperationXchg, Xchg),
  (FFI.rmwOperationAdd, Add),
  (FFI.rmwOperationSub, Sub),
  (FFI.rmwOperationAnd, And),
  (FFI.rmwOperationNand, Nand),
  (FFI.rmwOperationOr, Or),
  (FFI.rmwOperationXor, Xor),
  (FFI.rmwOperationMax, Max),
  (FFI.rmwOperationMin, Min),
  (FFI.rmwOperationUMax, UMax),
  (FFI.rmwOperationUMin, UMin)
 ]
