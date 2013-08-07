-- | Tools for JIT execution
module LLVM.General.ExecutionEngine (
  ExecutionEngine(..),
  ExecutableModule,
  JIT, withJIT,
  MCJIT, withMCJIT
  ) where

import LLVM.General.Internal.ExecutionEngine
