module LLVM.General.Internal.FFI.Transforms where

needsTargetLowering "CodeGenPrepare" = True
needsTargetLowering "LoopStrengthReduce" = True
needsTargetLowering "LowerInvoke" = True
needsTargetLowering _ = False

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
