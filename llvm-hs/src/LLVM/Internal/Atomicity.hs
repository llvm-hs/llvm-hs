{-# LANGUAGE
  TemplateHaskell,
  MultiParamTypeClasses
  #-}
module LLVM.Internal.Atomicity where

import LLVM.Prelude

import Data.Maybe

import qualified LLVM.Internal.FFI.LLVMCTypes as FFI

import LLVM.Internal.Coding
import qualified LLVM.AST as A

genCodingInstance [t| Maybe A.MemoryOrdering |] ''FFI.MemoryOrdering [
  (FFI.memoryOrderingNotAtomic, Nothing),
  (FFI.memoryOrderingUnordered, Just A.Unordered),
  (FFI.memoryOrderingMonotonic, Just A.Monotonic),
  (FFI.memoryOrderingAcquire, Just A.Acquire),
  (FFI.memoryOrderingRelease, Just A.Release),
  (FFI.memoryOrderingAcquireRelease, Just A.AcquireRelease),
  (FFI.memoryOrderingSequentiallyConsistent, Just A.SequentiallyConsistent)
 ]

genCodingInstance [t| A.SynchronizationScope |] ''FFI.SynchronizationScope [
  (FFI.synchronizationScopeSingleThread, A.SingleThread),
  (FFI.synchronizationScopeSystem, A.System)
 ]

instance Monad m => EncodeM m (Maybe A.Atomicity) (FFI.SynchronizationScope, FFI.MemoryOrdering) where
  encodeM a =
    return (,) `ap` encodeM (maybe A.SingleThread fst a) `ap` encodeM (liftM snd a)

instance Monad m => DecodeM m (Maybe A.Atomicity) (FFI.SynchronizationScope, FFI.MemoryOrdering) where
  decodeM (ss, ao) = return (liftM . (,)) `ap` decodeM ss `ap` decodeM ao

instance Monad m => EncodeM m A.Atomicity (FFI.SynchronizationScope, FFI.MemoryOrdering) where
  encodeM = encodeM . Just

instance Monad m => DecodeM m A.Atomicity (FFI.SynchronizationScope, FFI.MemoryOrdering) where
  decodeM = liftM fromJust . decodeM

instance Monad m => EncodeM m A.MemoryOrdering FFI.MemoryOrdering where
  encodeM = encodeM . Just

instance Monad m => DecodeM m A.MemoryOrdering FFI.MemoryOrdering where
  decodeM = liftM fromJust . decodeM

