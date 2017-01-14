-- | Tools for JIT execution
module LLVM.ExecutionEngine (
  ExecutionEngine(..),
  ExecutableModule,
  MCJIT, withMCJIT
  ) where

import LLVM.Internal.ExecutionEngine
