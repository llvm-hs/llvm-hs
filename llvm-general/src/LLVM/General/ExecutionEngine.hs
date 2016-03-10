-- | Tools for JIT execution
module LLVM.General.ExecutionEngine (
  ExecutionEngine(..),
  ExecutableModule,
  MCJIT, withMCJIT
  ) where

import LLVM.General.Internal.ExecutionEngine
