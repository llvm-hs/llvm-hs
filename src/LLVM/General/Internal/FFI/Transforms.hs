-- | Code used with Template Haskell to build the FFI for transform passes.
module LLVM.General.Internal.FFI.Transforms where

-- | does the constructor for this pass require a TargetLowering object
needsTargetLowering "CodeGenPrepare" = True
needsTargetLowering "LoopStrengthReduce" = True
needsTargetLowering "LowerInvoke" = True
needsTargetLowering _ = False

-- | Translate a Haskell name (used in the public Haskell interface, typically not abbreviated)
-- | for a pass into the (sometimes obscure, sometimes abbreviated) name used in the LLVM C interface.
-- | This translation includes, by choice of prefix, whether the C interface implementation is found in
-- | the LLVM distribution ("LLVM" prefix) or either not available or broken there and so implemented
-- | as part of this Haskell package ("LLVM_General_" prefix).
cName n = 
    let core = case n of
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
            h -> h
        patchImpls = [
         "CodeGenPrepare",
         "GlobalValueNumbering",
         "InternalizeFunctions",
         "BasicBlockVectorize",
	 "BlockPlacement",
	 "BreakCriticalEdges",
	 "DeadCodeElimination",
	 "DeadInstructionElimination",
	 "DemoteRegisterToMemory",
	 "LoopClosedSingleStaticAssignment",
	 "LoopInstructionSimplify",
         "LoopStrengthReduce",
	 "LowerAtomic",
	 "LowerInvoke",
	 "LowerSwitch",
	 "MergeFunctions",
	 "PartialInlining",
         "ScalarReplacementOfAggregates",
	 "Sinking",
	 "StripDeadDebugInfo",
	 "StripDebugDeclare",
	 "StripNonDebugSymbols"
         ]
    in
      (if (n `elem` patchImpls) then "LLVM_General_" else "LLVM") ++ "Add" ++ core ++ "Pass"
