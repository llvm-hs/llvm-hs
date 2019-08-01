-- | <http://llvm.org/doxygen/classllvm_1_1TargetOptions.html>
module LLVM.Target.Options where

import LLVM.Prelude

-- | <http://llvm.org/doxygen/namespacellvm_1_1FloatABI.html#aea077c52d84934aabf9445cef9eab2e2>
data FloatABI
  = FloatABIDefault
  | FloatABISoft
  | FloatABIHard
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Data, Generic)

-- | <http://llvm.org/doxygen/namespacellvm_1_1FPOpFusion.html#a9c71bae9f02af273833fde586d529fc5>
data FloatingPointOperationFusionMode
  = FloatingPointOperationFusionFast
  | FloatingPointOperationFusionStandard
  | FloatingPointOperationFusionStrict
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Data, Generic)

-- | <https://llvm.org/doxygen/namespacellvm.html#aa100a124c9d33561b0950011928aae00>
data DebugCompressionType
  = CompressNone -- ^ No compression
  | CompressGNU -- ^ zlib-gnu style compression
  | CompressZ -- ^ zlib style compression
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Data, Generic)

-- | <http://llvm.org/doxygen/namespacellvm_1_1ThreadModel.html#a299c775d35e28348ecfbe03c38c17fe1>
data ThreadModel
  = ThreadModelPOSIX
  | ThreadModelSingle
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Data, Generic)

-- | <http://llvm.org/doxygen/namespacellvm.html#adc04b17f40513e658e600a26842b1ed6>
data DebuggerKind
  = DebuggerDefault
  | DebuggerGDB
  | DebuggerLLDB
  | DebuggerSCE
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Data, Generic)

-- | <http://llvm.org/doxygen/namespacellvm.html#ada924e855250645672a493841803ff91>
data EABIVersion
  = EABIVersionUnknown
  | EABIVersionDefault
  | EABIVersion4
  | EABIVersion5
  | EABIVersionGNU
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Data, Generic)

-- | <http://llvm.org/doxygen/namespacellvm_1_1FPDenormal.html#aa0e896c04e0537cf6d0926f3c8db6d6c>
data FloatingPointDenormalMode
  = FloatingPointDenormalIEEE -- ^ IEEE 754 denormal numbers
  | FloatingPointDenormalPreserveSign -- ^ The sign of a flushed-to-zero number is preserved in the sign of 0
  | FloatingPointDenormalPositiveZero -- ^ Denormals are flushed to positive zero
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Data, Generic)

-- | <http://llvm.org/doxygen/namespacellvm.html#a2ca3855108426698ff21517a7c884c84>
data ExceptionHandling
  = ExceptionHandlingNone -- ^ No exception support
  | ExceptionHandlingDwarfCFI -- ^ DWARF-like instruction based exceptions
  | ExceptionHandlingSjLj -- ^ setjmp/longjmp based exceptions
  | ExceptionHandlingARM -- ^ ARM EHABI
  | ExceptionHandlingWinEH -- ^ Windows Exception Handling
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Data, Generic)

-- | The options of a 'LLVM.Target.TargetOptions'
-- <http://llvm.org/doxygen/classllvm_1_1TargetOptions.html>
data Options = Options {
  printMachineCode :: Bool,
  unsafeFloatingPointMath :: Bool,
  noInfinitiesFloatingPointMath :: Bool,
  noNaNsFloatingPointMath :: Bool,
  noTrappingFloatingPointMath :: Bool,
  noSignedZeroesFloatingPointMath :: Bool,
  honorSignDependentRoundingFloatingPointMathOption :: Bool,
  noZerosInBSS :: Bool,
  guaranteedTailCallOptimization :: Bool,
  stackSymbolOrdering :: Bool,
  enableFastInstructionSelection :: Bool,
  useInitArray :: Bool,
  disableIntegratedAssembler :: Bool,
  compressDebugSections :: DebugCompressionType,
  relaxELFRelocations :: Bool,
  functionSections :: Bool,
  dataSections :: Bool,
  uniqueSectionNames :: Bool,
  trapUnreachable :: Bool,
  emulatedThreadLocalStorage :: Bool,
  enableInterProceduralRegisterAllocation :: Bool,
  stackAlignmentOverride :: Word32,
  floatABIType :: FloatABI,
  allowFloatingPointOperationFusion :: FloatingPointOperationFusionMode,
  threadModel :: ThreadModel,
  eabiVersion :: EABIVersion,
  debuggerTuning :: DebuggerKind,
  floatingPointDenormalMode :: FloatingPointDenormalMode,
  exceptionModel :: ExceptionHandling,
  machineCodeOptions :: MachineCodeOptions
  }
  deriving (Eq, Ord, Read, Show)

-- | <http://llvm.org/doxygen/classllvm_1_1MCTargetOptions.html>
data MachineCodeOptions = MachineCodeOptions {
  relaxAll :: Bool,
  noExecutableStack :: Bool,
  fatalWarnings :: Bool,
  noWarnings :: Bool,
  noDeprecatedWarning :: Bool,
  saveTemporaryLabels :: Bool,
  useDwarfDirectory :: Bool,
  incrementalLinkerCompatible :: Bool,
  pieCopyRelocations :: Bool,
  showMachineCodeEncoding :: Bool,
  showMachineCodeInstructions :: Bool,
  verboseAssembly :: Bool,
  preserveComentsInAssembly :: Bool
  }
  deriving (Eq, Ord, Read, Show)
