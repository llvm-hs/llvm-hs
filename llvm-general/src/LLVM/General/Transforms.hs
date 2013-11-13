-- | This module provides an enumeration of the various transformation (e.g. optimization) passes
-- provided by LLVM. They can be used to create a 'LLVM.General.PassManager.PassManager' to, in turn,
-- run the passes on 'LLVM.General.Module.Module's. If you don't know what passes you want, consider
-- instead using 'LLVM.General.PassManager.CuratedPassSetSpec'.
module LLVM.General.Transforms where

import Data.Data
import Data.Word

-- | <http://llvm.org/docs/Passes.html#transform-passes>
-- A few passes can make use of information in a 'LLVM.General.Target.TargetMachine' if one
-- is provided to 'LLVM.General.PassManager.createPassManager'.
-- <http://llvm.org/doxygen/classllvm_1_1Pass.html>
data Pass
  -- here begin the Scalar passes
  = AggressiveDeadCodeElimination
  | BreakCriticalEdges
  -- | can use a 'LLVM.General.Target.TargetMachine'
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
  | LoopUnroll { loopUnrollThreshold :: Maybe Word, count :: Maybe Word, allowPartial :: Maybe Bool }
  | LoopUnswitch { optimizeForSize :: Bool }
  | LowerAtomic
  -- | can use a 'LLVM.General.Target.TargetMachine'
  | LowerInvoke { useExpensiveExceptionHandlingSupport :: Bool } 
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
      vectorBits :: Word,
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
      requiredChainDepth :: Word,
      searchLimit :: Word,
      maxCandidatePairsForCycleCheck :: Word,
      splatBreaksChain :: Bool,
      maxInstructions :: Word,
      maxIterations :: Word,
      powerOfTwoLengthsOnly :: Bool,
      noMemoryOperationBoost :: Bool,
      fastDependencyAnalysis :: Bool
    }
  | LoopVectorize

  -- here begin the instrumentation passes
  | GCOVProfiler {
      emitNotes :: Bool,
      emitData :: Bool,
      version :: GCOVVersion, 
      useCfgChecksum :: Bool,
      noRedZone :: Bool,
      functionNamesInData :: Bool
    }
  | AddressSanitizer {
      checkInitOrder :: Bool,
      checkUseAfterReturn :: Bool,
      checkLifetime :: Bool,
      blackListFile :: Maybe FilePath,
      zeroBaseShadow :: Bool
    }
  | AddressSanitizerModule {
      checkInitOrder :: Bool,
      blackListFile :: Maybe FilePath,
      zeroBaseShadow :: Bool
    }
  | MemorySanitizer {
      trackOrigins :: Bool,
      blackListFile :: Maybe FilePath
    }
  | ThreadSanitizer {
      blackListFile :: Maybe FilePath
    }
  | BoundsChecking
  | DebugGeneratedIR {
      hideDebugIntrinsics :: Bool,
      hideDebugMetadata :: Bool,
      fileName :: Maybe FilePath,
      directory :: Maybe FilePath
    }
  | DebugExistingIR
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

-- | See <http://gcc.gnu.org/viewcvs/gcc/trunk/gcc/gcov-io.h?view=markup>.
newtype GCOVVersion = GCOVVersion String
  deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | Defaults for 'GCOVProfiler'.
defaultGCOVProfiler = GCOVProfiler {
    emitNotes = True,
    emitData = True,
    version = GCOVVersion "402*", 
    useCfgChecksum = False,
    noRedZone = False,
    functionNamesInData = True
  }

-- | Defaults for 'AddressSanitizer'.
defaultAddressSanitizer = AddressSanitizer {
  checkInitOrder = True,
  checkUseAfterReturn = False,
  checkLifetime = False,
  blackListFile = Nothing,
  zeroBaseShadow = False
}

-- | Defaults for 'AddressSanitizerModule'.
defaultAddressSanitizerModule = AddressSanitizerModule {
  checkInitOrder = True,
  blackListFile = Nothing,
  zeroBaseShadow = False
}

-- | Defaults for 'MemorySanitizer'.
defaultMemorySanitizer = MemorySanitizer {
  trackOrigins = False,
  blackListFile = Nothing
}

-- | Defaults for 'ThreadSanitizer'.
defaultThreadSanitizer = ThreadSanitizer {
  blackListFile = Nothing
}

-- | Defaults for 'DebugGeneratedIR'.
defaultDebugGeneratedIR = DebugGeneratedIR {
  hideDebugIntrinsics = False,
  hideDebugMetadata = False,
  fileName = Nothing,
  directory = Nothing
}
