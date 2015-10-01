{-# LANGUAGE
  TemplateHaskell,
  MultiParamTypeClasses
  #-}
module LLVM.General.Internal.TailCallKind where

import LLVM.General.Prelude

import qualified LLVM.General.Internal.FFI.LLVMCTypes as FFI

import LLVM.General.Internal.Coding
import qualified LLVM.General.AST as A

genCodingInstance [t| Maybe A.TailCallKind |] ''FFI.TailCallKind [
  (FFI.tailCallKindNone, Nothing),
  (FFI.tailCallKindTail, Just A.Tail),
  (FFI.tailCallKindMustTail, Just A.MustTail)
 ]
