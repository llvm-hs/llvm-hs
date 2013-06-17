{-# LANGUAGE
  DeriveDataTypeable
  #-}
  
module LLVM.General.Transforms where

import Data.Data
import Data.Int
import Data.Word

data Pass
  -- here begin the Scalar passes
  = AggressiveDeadCodeElimination
  | BlockPlacement
  | BreakCriticalEdges
  | CodeGenPrepare
  | ConstantPropagation
  | CorrelatedValuePropagation
  | DeadCodeElimination
  | DeadInstructionElimination
  | DeadStoreElimination
  | DemoteRegisterToMemory
  | EarlyCommonSubexpressionElimination
  | GlobalValueNumbering { noLoads :: Bool }
  | InductionVariableSimplify
  | InstructionCombining
  | JumpThreading
  | LoopClosedSingleStaticAssignment
  | LoopInvariantCodeMotion
  | LoopDeletion
  | LoopIdiom
  | LoopInstructionSimplify
  | LoopRotate
  | LoopStrengthReduce
  | LoopUnroll { loopUnrollThreshold :: Int32, count :: Int32, allowPartial :: Int32 }
  | LoopUnswitch { optimizeForSize :: Bool }
  | LowerAtomic
  | LowerInvoke { useExpensiveExceptionHandlingSupport :: Bool }
  | LowerSwitch
  | LowerExpectIntrinsic
  | MemcpyOptimization
  | PromoteMemoryToRegister
  | Reassociate
  | ScalarReplacementOfAggregates { requiresDominatorTree :: Bool }
  | OldScalarReplacementOfAggregates { 
      oldScalarReplacementOfAggregatesThreshold :: Int32, 
      useDominatorTree :: Bool, 
      structMemberThreshold :: Int32,
      arrayElementThreshold :: Int32,
      scalarLoadThreshold :: Int32
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
      functionInliningThreshold :: Int32
    }
  | GlobalDeadCodeElimination
  | InternalizeFunctions { exportList :: [String] }
  | InterproceduralConstantPropagation
  | InterproceduralSparseConditionalConstantPropagation
  | MergeFunctions
  | PartialInlining
  | PruneExceptionHandling
  | StripDeadDebugInfo
  | StripDebugDeclare
  | StripNonDebugSymbols
  | StripSymbols { onlyDebugInfo :: Bool }

  -- here begin the vectorization passes
  | BasicBlockVectorize { 
    vectorBits :: Word32,
    vectorizeBools :: Bool,
    vectorizeInts :: Bool,
    vectorizeFloats :: Bool,
    vectorizePointers :: Bool,
    vectorizeCasts :: Bool,
    vectorizeMath :: Bool,
    vectorizeFusedMultiplyAdd :: Bool,
    vectorizeSelect :: Bool,
    vectorizeCmp :: Bool,
    vectorizeGetElementPtr :: Bool,
    vectorizeMemoryOperations :: Bool,
    alignedOnly :: Bool,
    requiredChainDepth :: Word32,
    searchLimit :: Word32,
    maxCandidatePairsForCycleCheck :: Word32,
    splatBreaksChain :: Bool,
    maxInstructions :: Word32,
    maxIterations :: Word32,
    powerOfTwoLengthsOnly :: Bool,
    noMemoryOperationBoost :: Bool,
    fastDependencyAnalysis :: Bool
    }
  | LoopVectorize
  deriving (Eq, Ord, Read, Show, Typeable, Data)

defaultVectorizeBasicBlocks = BasicBlockVectorize {
    vectorBits = 128,
    vectorizeBools = True,
    vectorizeInts = True,
    vectorizeFloats = True,
    vectorizePointers = True,
    vectorizeCasts = True,
    vectorizeMath = True,
    vectorizeFusedMultiplyAdd = True,
    vectorizeSelect = True,
    vectorizeCmp = True,
    vectorizeGetElementPtr = True,
    vectorizeMemoryOperations = True,
    alignedOnly = True,

    requiredChainDepth = 6,
    searchLimit = 400,
    maxCandidatePairsForCycleCheck = 200,
    splatBreaksChain = False,
    maxInstructions = 500,
    maxIterations = 0,
    powerOfTwoLengthsOnly = False,
    noMemoryOperationBoost = False,
    fastDependencyAnalysis = False
  }
