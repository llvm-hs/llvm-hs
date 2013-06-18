-- | Tools for JIT execution
module LLVM.General.ExecutionEngine (
  ExecutionEngine,
  withExecutionEngine,
  withModuleInEngine,
  findFunction
  ) where

import LLVM.General.Internal.ExecutionEngine
