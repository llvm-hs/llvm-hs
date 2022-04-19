{-# LANGUAGE OverloadedStrings #-}
-- | This module provides an enumeration of the various transformation (e.g. optimization) passes
-- provided by LLVM. They can be used to create a 'LLVM.PassManager.PassManager' to, in turn,
-- run the passes on 'LLVM.Module.Module's. If you don't know what passes you want, consider
-- instead using 'LLVM.PassManager.CuratedPassSetSpec'.
module LLVM.Transforms where

import LLVM.Prelude

-- | <http://llvm.org/docs/Passes.html#transform-passes>
-- A few passes can make use of information in a 'LLVM.Target.TargetMachine' if one
-- is provided to 'LLVM.PassManager.createPassManager'.
-- <http://llvm.org/doxygen/classllvm_1_1Pass.html>
data Pass
  -- here begin the Scalar passes
  = AggressiveDeadCodeElimination
  | BreakCriticalEdges
  -- | can use a 'LLVM.Target.TargetMachine'
  | CodeGenPrepare
  | CorrelatedValuePropagation
  | DeadCodeElimination
  | DeadStoreElimination
  | DemoteRegisterToMemory
  | EarlyCommonSubexpressionElimination
  | GlobalValueNumbering { noLoads :: Bool }
  | InductionVariableSimplify
  | InstructionCombining
  -- | Instruction simplification includes constant folding
  | InstructionSimplify
  | JumpThreading
  | LoopClosedSingleStaticAssignment
  | LoopInvariantCodeMotion
  | LoopDeletion
  | LoopIdiom
  | LoopInstructionSimplify
  | LoopRotate
  | LoopStrengthReduce
  | LoopUnroll { loopUnrollThreshold :: Maybe Word, count :: Maybe Word, allowPartial :: Maybe Bool }
  | LoopUnswitch { optimizeForSize :: Bool }
  | LowerAtomic
  | LowerInvoke
  | LowerSwitch
  | LowerExpectIntrinsic
  | MemcpyOptimization
  | PromoteMemoryToRegister
  | Reassociate
  | ScalarReplacementOfAggregates { requiresDominatorTree :: Bool }
  | OldScalarReplacementOfAggregates {
      oldScalarReplacementOfAggregatesThreshold :: Maybe Word,
      useDominatorTree :: Bool,
      structMemberThreshold :: Maybe Word,
      arrayElementThreshold :: Maybe Word,
      scalarLoadThreshold :: Maybe Word
    }
  | SparseConditionalConstantPropagation
  | SimplifyLibCalls
  | SimplifyControlFlowGraph
  | Sinking
  | TailCallElimination

  -- here begin the Interprocedural passes
  | AlwaysInline { insertLifetime :: Bool }
  | ArgumentPromotion
  | ConstantMerge
  | FunctionAttributes
  | FunctionInlining {
      functionInliningThreshold :: Word
    }
  | GlobalDeadCodeElimination
  | InternalizeFunctions { exportList :: [String] }
  | InterproceduralSparseConditionalConstantPropagation
  | MergeFunctions
  | PartialInlining
  | PruneExceptionHandling
  | StripDeadDebugInfo
  | StripDebugDeclare
  | StripNonDebugSymbols
  | StripSymbols { onlyDebugInfo :: Bool }

  -- here begin the vectorization passes
  | LoopVectorize {
      interleaveOnlyWhenForced :: Bool,
      vectorizeOnlyWhenForced :: Bool
    }
  | SuperwordLevelParallelismVectorize
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | Defaults for the 'LoopVectorize' pass
defaultLoopVectorize :: Pass
defaultLoopVectorize = LoopVectorize {
    interleaveOnlyWhenForced = False,
    vectorizeOnlyWhenForced = False
  }
