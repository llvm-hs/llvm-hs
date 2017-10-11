{-# LANGUAGE
  TemplateHaskell,
  MultiParamTypeClasses
  #-}
module LLVM.Internal.TailCallKind where

import LLVM.Prelude

import qualified LLVM.Internal.FFI.LLVMCTypes as FFI

import LLVM.Internal.Coding
import qualified LLVM.AST as A

genCodingInstance [t| Maybe A.TailCallKind |] ''FFI.TailCallKind [
  (FFI.tailCallKindNone, Nothing),
  (FFI.tailCallKindTail, Just A.Tail),
  (FFI.tailCallKindMustTail, Just A.MustTail),
  (FFI.tailCallKindNoTail, Just A.NoTail)
 ]
