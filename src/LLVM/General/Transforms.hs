-- | This module provides an enumeration of the various transformation (e.g. optimization) passes
-- provided by LLVM. They can be used to create a 'LLVM.General.PassManager.PassManager' to, in turn,
-- run the passes on 'LLVM.General.Module.Module's. If you don't know what passes you want, consider
-- instead using 'LLVM.General.PassManager.CuratedPassSetSpec'.
module LLVM.General.Transforms where

import Data.Data
import Data.Int
import Data.Word

-- | <http://llvm.org/docs/Passes.html#transform-passes>
-- A few passes can make use of information in a 'LLVM.General.Target.TargetLowering' if one
-- is provided to 'LLVM.General.PassManager.createPassManager'.
-- <http://llvm.org/doxygen/classllvm_1_1Pass.html>
data Pass
  -- here begin the Scalar passes
  = AggressiveDeadCodeElimination
  | BlockPlacement
  | BreakCriticalEdges
  -- | can use a 'LLVM.General.Target.TargetLowering'
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
  -- | can use a 'LLVM.General.Target.TargetLowering'
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

-- | Defaults for the 'BasicBlockVectorize' pass - copied from the C++ code to keep these defaults
-- constant. (The C++ defaults are modifiable through global objects used for command-line processing,
-- in a design apparently oblivious to uses of LLVM besides the standard command-line tools).
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
