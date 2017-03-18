-- | Code used with Template Haskell to build the FFI for transform passes.
module LLVM.Internal.FFI.Transforms where

import LLVM.Prelude

-- | does the constructor for this pass require a TargetMachine object
needsTargetMachine :: String -> Bool
needsTargetMachine "CodeGenPrepare" = True
needsTargetMachine _ = False

-- | Translate a Haskell name (used in the public Haskell interface, typically not abbreviated)
-- | for a pass into the (sometimes obscure, sometimes abbreviated) name used in the LLVM C interface.
-- | This translation includes, by choice of prefix, whether the C interface implementation is found in
-- | the LLVM distribution ("LLVM" prefix) or either not available or broken there and so implemented
-- | as part of this Haskell package ("LLVM_Hs_" prefix).
cName :: String -> String
cName n =
    let core = case n of
            "AddressSanitizer" -> "AddressSanitizerFunction"
            "AggressiveDeadCodeElimination" -> "AggressiveDCE"
            "AlwaysInline" -> "AlwaysInliner"
            "DeadInstructionElimination" -> "DeadInstElimination"
            "EarlyCommonSubexpressionElimination" -> "EarlyCSE"
            "FunctionAttributes" -> "FunctionAttrs"
            "GlobalDeadCodeElimination" -> "GlobalDCE"
            "InductionVariableSimplify" -> "IndVarSimplify"
            "InternalizeFunctions" -> "Internalize"
            "InterproceduralConstantPropagation" -> "IPConstantPropagation"
            "InterproceduralSparseConditionalConstantPropagation" -> "IPSCCP"
            "LoopClosedSingleStaticAssignment" -> "LCSSA"
            "LoopInvariantCodeMotion" -> "LICM"
            "LoopInstructionSimplify" -> "LoopInstSimplify"
            "MemcpyOptimization" -> "MemCpyOpt"
            "PruneExceptionHandling" -> "PruneEH"
            "ScalarReplacementOfAggregates" -> "SROA"
            "OldScalarReplacementOfAggregates" -> "ScalarReplAggregates"
            "SimplifyControlFlowGraph" -> "CFGSimplification"
            "SparseConditionalConstantPropagation" -> "SCCP"
            "SuperwordLevelParallelismVectorize" -> "SLPVectorize"
            h -> h
        patchImpls = [
         "AddressSanitizer",
         "AddressSanitizerModule",
         "BoundsChecking",
         "CodeGenPrepare",
         "GlobalValueNumbering",
         "InternalizeFunctions",
         "BasicBlockVectorize",
         "BlockPlacement",
         "BreakCriticalEdges",
         "DeadCodeElimination",
         "DeadInstructionElimination",
         "DemoteRegisterToMemory",
         "EdgeProfiler",
         "GCOVProfiler",
         "LoopClosedSingleStaticAssignment",
         "LoopInstructionSimplify",
         "LoopStrengthReduce",
         "LoopVectorize",
         "LowerAtomic",
         "LowerInvoke",
         "LowerSwitch",
         "MemorySanitizer",
         "MergeFunctions",
         "OptimalEdgeProfiler",
         "PathProfiler",
         "PartialInlining",
         "ScalarReplacementOfAggregates",
         "Sinking",
         "StripDeadDebugInfo",
         "StripDebugDeclare",
         "StripNonDebugSymbols",
         "ThreadSanitizer"
         ]
    in
      (if (n `elem` patchImpls) then "LLVM_Hs_" else "LLVM") ++ "Add" ++ core ++ "Pass"
