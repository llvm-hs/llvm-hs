{-# LANGUAGE
  TemplateHaskell,
  MultiParamTypeClasses
  #-}
module LLVM.General.Internal.Atomicity where

import Control.Monad

import Data.Maybe

import qualified LLVM.General.Internal.FFI.LLVMCTypes as FFI

import LLVM.General.Internal.Coding

import qualified LLVM.General.AST as A

genCodingInstance [t| Maybe A.MemoryOrdering |] ''FFI.MemoryOrdering [
  (FFI.memoryOrderingNotAtomic, Nothing),
  (FFI.memoryOrderingUnordered, Just A.Unordered),
  (FFI.memoryOrderingMonotonic, Just A.Monotonic),
  (FFI.memoryOrderingAcquire, Just A.Acquire),
  (FFI.memoryOrderingRelease, Just A.Release),
  (FFI.memoryOrderingAcquireRelease, Just A.AcquireRelease),
  (FFI.memoryOrderingSequentiallyConsistent, Just A.SequentiallyConsistent)
 ]

instance Monad m => EncodeM m (Maybe A.Atomicity) (FFI.LLVMBool, FFI.MemoryOrdering) where
  encodeM a =
    return (,) `ap` encodeM (maybe False A.crossThread a) `ap` encodeM (liftM A.memoryOrdering a)

instance Monad m => DecodeM m (Maybe A.Atomicity) (FFI.LLVMBool, FFI.MemoryOrdering) where
  decodeM (ss, ao) = return (liftM . A.Atomicity) `ap` decodeM ss `ap` decodeM ao

instance Monad m => EncodeM m A.Atomicity (FFI.LLVMBool, FFI.MemoryOrdering) where
  encodeM = encodeM . Just

instance Monad m => DecodeM m A.Atomicity (FFI.LLVMBool, FFI.MemoryOrdering) where
  decodeM = liftM fromJust . decodeM

