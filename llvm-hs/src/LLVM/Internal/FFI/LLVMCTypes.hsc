{-# LANGUAGE
  GeneralizedNewtypeDeriving,
  PatternSynonyms
  #-}
-- | Define types which correspond cleanly with some simple types on the C/C++ side.
-- Encapsulate hsc macro weirdness here, supporting higher-level tricks elsewhere.
module LLVM.Internal.FFI.LLVMCTypes where

import LLVM.Prelude

#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#endif
#include "llvm-c/Core.h"
#include "llvm-c/Linker.h"
#include "llvm-c/OrcBindings.h"
#include "llvm-c/Target.h"
#include "llvm-c/TargetMachine.h"
#include "LLVM/Internal/FFI/Attribute.h"
#include "LLVM/Internal/FFI/Instruction.h"
#include "LLVM/Internal/FFI/Value.h"
#include "LLVM/Internal/FFI/SMDiagnostic.h"
#include "LLVM/Internal/FFI/InlineAssembly.h"
#include "LLVM/Internal/FFI/Target.h"
#include "LLVM/Internal/FFI/CallingConvention.h"
#include "LLVM/Internal/FFI/GlobalValue.h"
#include "LLVM/Internal/FFI/Type.h"
#include "LLVM/Internal/FFI/Constant.h"
#include "LLVM/Internal/FFI/Analysis.h"
#include "LLVM/Internal/FFI/LibFunc.h"
#include "LLVM/Internal/FFI/OrcJIT.h"
#include "LLVM/Internal/FFI/Metadata.h"

import Language.Haskell.TH.Quote

import Data.Bits
import Foreign.C
import Foreign.Storable

#{
define hsc_inject(l, typ, cons, hprefix, recmac) { \
  struct { const char *s; unsigned n; } *p, list[] = { LLVM_HS_FOR_EACH_ ## l(recmac) }; \
  for(p = list; p < list + sizeof(list)/sizeof(list[0]); ++p) { \
    hsc_printf(#hprefix "%s :: " #typ "\n", p->s); \
    hsc_printf(#hprefix "%s = " #cons " %u\n", p->s, p->n); \
  } \
  hsc_printf(#hprefix "P :: QuasiQuoter\n" \
             #hprefix "P = QuasiQuoter {\n" \
             "  quoteExp = undefined,\n" \
             "  quotePat = \\s -> dataToPatQ (const Nothing) $ case s of"); \
  for(p = list; p < list + sizeof(list)/sizeof(list[0]); ++p) { \
    hsc_printf("\n    \"%s\" -> " #hprefix "%s", p->s, p->s); \
  } \
  hsc_printf("\n    x -> error $ \"bad quasiquoted FFI constant for " #hprefix ": \" ++ x"); \
  hsc_printf(",\n" \
             "  quoteType = undefined,\n" \
             "  quoteDec = undefined\n" \
             " }\n"); \
}
}

#{
define hsc_patsyn(l, typ, cons, hprefix, recmac) { \
  struct { const char *s; unsigned n; } *p, list[] = { LLVM_HS_FOR_EACH_ ## l(recmac) }; \
  for(p = list; p < list + sizeof(list)/sizeof(list[0]); ++p) { \
    hsc_printf("pattern " #hprefix "%s :: " #typ "\n", p->s); \
    hsc_printf("pattern " #hprefix "%s =  " #cons " %u\n", p->s, p->n); \
  } \
}
}

#define DW_OP_Rec(n) { #n, LLVM_Hs_DwOp_##n },
#{patsyn DW_OP, Word64, , DwOp_, DW_OP_Rec}

newtype Encoding = Encoding CUInt
  deriving (Data, Show)

#define DW_ATE_Rec(n) { #n, LLVM_Hs_DwAtE_##n },
#{patsyn DW_ATE, Encoding, Encoding, DwAtE_, DW_ATE_Rec}

newtype DwTag = DwTag Word16
  deriving (Data, Show)

#define DW_TAG_Rec(n) { #n, LLVM_Hs_DwTag_##n },
#{patsyn DW_TAG, DwTag, DwTag, DwTag_, DW_TAG_Rec}

newtype DwVirtuality = DwVirtuality Word8
  deriving (Data, Show)

#define DW_VIRTUALITY_Rec(n) { #n, LLVM_Hs_DwVirtuality_##n },
#{patsyn DW_VIRTUALITY, DwVirtuality, DwVirtuality, DwVirtuality_, DW_VIRTUALITY_Rec}

deriving instance Data CUInt

newtype LLVMBool = LLVMBool CUInt

-- | If an FFI function returns a value wrapped in 'OwnerTransfered',
-- this value needs to be freed after it has been processed. Usually
-- this is done automatically in the 'DecodeM' instance.
newtype OwnerTransfered a = OwnerTransfered a
  deriving (Eq, Storable)

newtype NothingAsMinusOne h = NothingAsMinusOne CInt
  deriving (Storable)

newtype NothingAsEmptyString c = NothingAsEmptyString c
  deriving (Storable)

newtype CPPOpcode = CPPOpcode CUInt
  deriving (Eq, Ord, Show, Typeable, Data, Generic)

newtype ICmpPredicate = ICmpPredicate CUInt
  deriving (Eq, Ord, Show, Typeable, Data, Generic)
#{enum ICmpPredicate, ICmpPredicate,
 iCmpPredEQ = LLVMIntEQ,
 iCmpPredNE = LLVMIntNE,
 iCmpPredUGT = LLVMIntUGT,
 iCmpPredUGE = LLVMIntUGE,
 iCmpPredULT = LLVMIntULT,
 iCmpPredULE = LLVMIntULE,
 iCmpPredSGT = LLVMIntSGT,
 iCmpPredSGE = LLVMIntSGE,
 iCmpPredSLT = LLVMIntSLT,
 iCmpPredSLE = LLVMIntSLE
}

newtype FCmpPredicate = FCmpPredicate CUInt
  deriving (Eq, Ord, Show, Typeable, Data, Generic)
#{enum FCmpPredicate, FCmpPredicate,
 fCmpPredFalse = LLVMRealPredicateFalse,
 fCmpPredOEQ = LLVMRealOEQ,
 fCmpPredOGT = LLVMRealOGT,
 fCmpPredOGE = LLVMRealOGE,
 fCmpPredOLT = LLVMRealOLT,
 fCmpPredOLE = LLVMRealOLE,
 fCmpPredONE = LLVMRealONE,
 fCmpPredORD = LLVMRealORD,
 fCmpPredUNO = LLVMRealUNO,
 fCmpPredUEQ = LLVMRealUEQ,
 fCmpPredUGT = LLVMRealUGT,
 fCmpPredUGE = LLVMRealUGE,
 fCmpPredULT = LLVMRealULT,
 fCmpPredULE = LLVMRealULE,
 fCmpPredUNE = LLVMRealUNE,
 fcmpPredTrue = LLVMRealPredicateTrue
}

newtype MDKindID = MDKindID CUInt
  deriving (Storable)

newtype MDSubclassID = MDSubclassID CUInt
  deriving (Eq, Read, Show, Typeable, Data, Generic)
#define MDSID_Rec(n) { #n, n ## Kind },
#{inject MDNODE_SUBCLASS, MDSubclassID, MDSubclassID, mdSubclassId, MDSID_Rec}

newtype FastMathFlags = FastMathFlags CUInt
  deriving (Eq, Ord, Show, Typeable, Data, Num, Bits, Generic)
#define FMF_Rec(n,l,ignored) { #n, LLVM ## n, },
#{inject FAST_MATH_FLAG, FastMathFlags, FastMathFlags, fastMathFlags, FMF_Rec}

newtype MemoryOrdering = MemoryOrdering CUInt
  deriving (Eq, Show, Typeable, Data, Generic)
#define MO_Rec(n) { #n, LLVMAtomicOrdering ## n },
#{inject ATOMIC_ORDERING, MemoryOrdering, MemoryOrdering, memoryOrdering, MO_Rec}

newtype UnnamedAddr = UnnamedAddr CUInt
  deriving (Eq, Show, Typeable, Data, Generic)
#define UA_Rec(n) { #n, LLVM ## n ## UnnamedAddr },
#{inject UNNAMED_ADDR, UnnamedAddr, UnnamedAddr, unnamedAddr, UA_Rec}

newtype SynchronizationScope = SynchronizationScope CUInt
  deriving (Eq, Show, Typeable, Data, Generic)
#define SS_Rec(n) { #n, LLVM ## n ## SynchronizationScope },
#{inject SYNCRONIZATION_SCOPE, SynchronizationScope, SynchronizationScope, synchronizationScope, SS_Rec}

newtype TailCallKind = TailCallKind CUInt
  deriving (Eq, Show, Typeable, Data, Generic)
#define TCK_Rec(n) { #n, LLVM_Hs_TailCallKind_ ## n },
#{inject TAIL_CALL_KIND, TailCallKind, TailCallKind, tailCallKind, TCK_Rec}

newtype Linkage = Linkage CUInt
  deriving (Eq, Read, Show, Typeable, Data, Generic)
#define LK_Rec(n) { #n, LLVM ## n ## Linkage },
#{inject LINKAGE, Linkage, Linkage, linkage, LK_Rec}

newtype Visibility = Visibility CUInt
  deriving (Eq, Read, Show, Typeable, Data, Generic)
#define VIS_Rec(n) { #n, LLVM ## n ## Visibility },
#{inject VISIBILITY, Visibility, Visibility, visibility, VIS_Rec}

newtype COMDATSelectionKind = COMDATSelectionKind CUInt
  deriving (Eq, Read, Show, Typeable, Data, Generic)
#define CSK(n) { #n, LLVM_Hs_COMDAT_Selection_Kind_ ## n },
#{inject COMDAT_SELECTION_KIND, COMDATSelectionKind, COMDATSelectionKind, comdatSelectionKind, CSK}

newtype DLLStorageClass = DLLStorageClass CUInt
  deriving (Eq, Read, Show, Typeable, Data, Generic)
#define DLLSC_Rec(n) { #n, LLVM ## n ## StorageClass },
#{inject DLL_STORAGE_CLASS, DLLStorageClass, DLLStorageClass, dllStorageClass, DLLSC_Rec}

newtype CallingConvention = CallingConvention CUInt
  deriving (Eq, Read, Show, Typeable, Data, Generic)
#define CC_Rec(l, n) { #l, LLVM_Hs_CallingConvention_ ## l },
#{inject CALLING_CONVENTION, CallingConvention, CallingConvention, callingConvention, CC_Rec}

newtype ThreadLocalMode = ThreadLocalMode CUInt
  deriving (Eq, Read, Show, Typeable, Data, Generic)
#define TLS_Rec(n) { #n, LLVM ## n },
#{inject THREAD_LOCAL_MODE, ThreadLocalMode, ThreadLocalMode, threadLocalMode, TLS_Rec}

newtype ValueSubclassId = ValueSubclassId CUInt
  deriving (Eq, Read, Show, Typeable, Data, Generic)
#define VSID_Rec(n) { #n, LLVM ## n ## SubclassId },
#{inject VALUE_SUBCLASS, ValueSubclassId, ValueSubclassId, valueSubclassId, VSID_Rec}

newtype DiagnosticKind = DiagnosticKind CUInt
  deriving (Eq, Read, Show, Typeable, Data, Generic)
#define DK_Rec(n) { #n, LLVMDiagnosticKind ## n },
#{inject DIAGNOSTIC_KIND, DiagnosticKind, DiagnosticKind, diagnosticKind, DK_Rec}

newtype AsmDialect = AsmDialect CUInt
  deriving (Eq, Read, Show, Typeable, Data, Generic)
#define ASM_Rec(n) { #n, LLVMAsmDialect_ ## n },
#{inject ASM_DIALECT, AsmDialect, AsmDialect, asmDialect, ASM_Rec}

newtype RMWOperation = RMWOperation CUInt
  deriving (Eq, Read, Show, Typeable, Data, Generic)
#define RMWOp_Rec(n) { #n, LLVMAtomicRMWBinOp ## n },
#{inject RMW_OPERATION, RMWOperation, RMWOperation, rmwOperation, RMWOp_Rec}

newtype RelocModel = RelocModel CUInt
  deriving (Eq, Read, Show, Typeable, Data, Generic)
#define RM_Rec(n,m) { #n, LLVMReloc ## n },
#{inject RELOC_MODEL, RelocModel, RelocModel, relocModel, RM_Rec}

newtype CodeModel = CodeModel CUInt
  deriving (Eq, Read, Show, Typeable, Data, Generic)
#define CM_Rec(n) { #n, LLVMCodeModel ## n },
#{inject CODE_MODEL, CodeModel, CodeModel, codeModel, CM_Rec}

newtype CodeGenOptLevel = CodeGenOptLevel CUInt
  deriving (Eq, Read, Show, Typeable, Data, Generic)
#define CGOL_Rec(n) { #n, LLVMCodeGenLevel ## n },
#{inject CODE_GEN_OPT_LEVEL, CodeGenOptLevel, CodeGenOptLevel, codeGenOptLevel, CGOL_Rec}

newtype CodeGenFileType = CodeGenFileType CUInt
  deriving (Eq, Read, Show, Typeable, Data, Generic)
#define CGFT_Rec(n) { #n, LLVM ## n ## File },
#{inject CODE_GEN_FILE_TYPE, CodeGenFileType, CodeGenFileType, codeGenFileType, CGFT_Rec}

newtype FloatABIType = FloatABIType CUInt
  deriving (Eq, Read, Show, Typeable, Data, Generic)
#define FAT_Rec(n) { #n, LLVM_Hs_FloatABI_ ## n },
#{inject FLOAT_ABI, FloatABIType, FloatABIType, floatABI, FAT_Rec}

newtype FPOpFusionMode = FPOpFusionMode CUInt
  deriving (Eq, Read, Show, Typeable, Data, Generic)
#define FPOFM_Rec(n) { #n, LLVM_Hs_FPOpFusionMode_ ## n },
#{inject FP_OP_FUSION_MODE, FPOpFusionMode, FPOpFusionMode, fpOpFusionMode, FPOFM_Rec}

newtype ThreadModel = ThreadModel CUInt
  deriving (Eq, Read, Show, Typeable, Data, Generic)
#define TM_Rec(n) { #n, LLVM_Hs_ThreadModel_ ## n },
#{inject THREAD_MODEL, ThreadModel, ThreadModel, threadModel, TM_Rec}

newtype EABI = EABI CUInt
  deriving (Eq, Read, Show, Typeable, Data, Generic)
#define EABI_Rec(n) { #n, LLVM_Hs_EABI_ ## n },
#{inject EABI, EABI, EABI, eabiVersion, EABI_Rec}

newtype DebuggerKind = DebuggerKind CUInt
  deriving (Eq, Read, Show, Typeable, Data, Generic)
#define DBGK_Rec(n) { #n, LLVM_Hs_DebuggerKind_ ## n },
#{inject DEBUGGER_KIND, DebuggerKind, DebuggerKind, debuggerKind, DBGK_Rec}

newtype FPDenormalMode = FPDenormalMode CUInt
  deriving (Eq, Read, Show, Typeable, Data, Generic)
#define FPDM_Rec(n) { #n, LLVM_Hs_FPDenormalMode_ ## n },
#{inject FP_DENORMAL_MODE, FPDenormalMode, FPDenormalMode, fpDenormalMode, FPDM_Rec}

newtype ExceptionHandling = ExceptionHandling CUInt
  deriving (Eq, Read, Show, Typeable, Data, Generic)
#define EH_Rec(n) { #n, LLVM_Hs_ExceptionHandling_ ## n },
#{inject EXCEPTION_HANDLING, ExceptionHandling, ExceptionHandling, exceptionHandling, EH_Rec}

newtype TargetOptionFlag = TargetOptionFlag CUInt
  deriving (Eq, Read, Show, Typeable, Data, Generic)
#define TOF_Rec(n) { #n, LLVM_Hs_TargetOptionFlag_ ## n },
#{inject TARGET_OPTION_FLAG, TargetOptionFlag, TargetOptionFlag, targetOptionFlag, TOF_Rec}

newtype MCTargetOptionFlag = MCTargetOptionFlag CUInt
  deriving (Eq, Read, Show, Typeable, Data, Generic)
#define MCTOF_Rec(n) { #n, LLVM_Hs_MCTargetOptionFlag_ ## n },
#{inject MC_TARGET_OPTION_FLAG, MCTargetOptionFlag, MCTargetOptionFlag, mcTargetOptionFlag, MCTOF_Rec}

newtype DebugCompressionType = DebugCompressionType CUInt
  deriving (Eq, Read, Show, Typeable, Data, Generic)
#define DCT_Rec(n) { #n, LLVM_Hs_DebugCompressionType_ ## n },
#{inject DEBUG_COMPRESSION_TYPE, DebugCompressionType, DebugCompressionType, debugCompressionType, DCT_Rec}

newtype TypeKind = TypeKind CUInt
  deriving (Eq, Read, Show, Typeable, Data, Generic)
#define TK_Rec(n) { #n, LLVM ## n ## TypeKind },
#{inject TYPE_KIND, TypeKind, TypeKind, typeKind, TK_Rec}

#define COMMA ,
#define IF_T(z) z
#define IF_F(z)
#define IF2(x) IF_ ## x
#define IF(x) IF2(x)
#define OR_TT T
#define OR_TF T
#define OR_FT T
#define OR_FF F
#define OR(x,y) OR_ ## x ## y
newtype ParameterAttributeKind = ParameterAttributeKind CUInt
  deriving (Eq, Read, Show, Typeable, Data, Generic)
#define PAK_Rec(n,p,r,f) IF(OR(p,r))({ #n COMMA LLVM_Hs_AttributeKind_ ## n} COMMA)
#{inject ATTRIBUTE_KIND, ParameterAttributeKind, ParameterAttributeKind, parameterAttributeKind, PAK_Rec}

newtype FunctionAttributeKind = FunctionAttributeKind CUInt
  deriving (Eq, Read, Show, Typeable, Data, Generic)
#define FAK_Rec(n,p,r,f) IF(f)({ #n COMMA LLVM_Hs_AttributeKind_ ## n} COMMA)
#{inject ATTRIBUTE_KIND, FunctionAttributeKind, FunctionAttributeKind, functionAttributeKind, FAK_Rec}

newtype FloatSemantics = FloatSemantics CUInt
  deriving (Eq, Read, Show, Typeable, Data, Generic)
#define FS_Rec(n) { #n, LLVMFloatSemantics ## n },
#{inject FLOAT_SEMANTICS, FloatSemantics, FloatSemantics, floatSemantics, FS_Rec}

newtype VerifierFailureAction = VerifierFailureAction CUInt
  deriving (Eq, Read, Show, Bits, Typeable, Data, Num, Generic)
#define VFA_Rec(n) { #n, LLVM ## n ## Action },
#{inject VERIFIER_FAILURE_ACTION, VerifierFailureAction, VerifierFailureAction, verifierFailureAction, VFA_Rec}

newtype LibFunc = LibFunc CUInt
  deriving (Eq, Read, Show, Bits, Typeable, Data, Num, Storable, Generic)
#define LF_Rec(n) { #n, LLVMLibFunc__ ## n },
#{inject LIB_FUNC, LibFunc, LibFunc, libFunc__, LF_Rec}

newtype JITSymbolFlags = JITSymbolFlags CUInt
  deriving (Eq, Read, Show, Bits, Typeable, Data, Num, Storable, Generic)
#define SF_Rec(n) { #n, LLVMJITSymbolFlag ## n },
#{inject JIT_SYMBOL_FLAG, JITSymbolFlags, JITSymbolFlags, jitSymbolFlags, SF_Rec}

newtype ChecksumKind = ChecksumKind CUInt
  deriving (Data, Show)

newtype Macinfo = Macinfo CUInt
  deriving (Data, Show)

pattern DW_Macinfo_Define :: Macinfo
pattern DW_Macinfo_Define = Macinfo 0x01
pattern DW_Macinfo_Undef :: Macinfo
pattern DW_Macinfo_Undef = Macinfo 0x02

newtype DebugEmissionKind = DebugEmissionKind CUInt
  deriving (Data, Show)

pattern NoDebug :: DebugEmissionKind
pattern NoDebug = DebugEmissionKind 0
pattern FullDebug :: DebugEmissionKind
pattern FullDebug = DebugEmissionKind 1
pattern LineTablesOnly :: DebugEmissionKind
pattern LineTablesOnly = DebugEmissionKind 2

newtype DebugNameTableKind = DebugNameTableKind CUInt
  deriving (Data, Show)

pattern NameTableKindDefault :: DebugNameTableKind
pattern NameTableKindDefault = DebugNameTableKind 0

pattern NameTableKindGNU :: DebugNameTableKind
pattern NameTableKindGNU = DebugNameTableKind 1

pattern NameTableKindNone :: DebugNameTableKind
pattern NameTableKindNone = DebugNameTableKind 2
