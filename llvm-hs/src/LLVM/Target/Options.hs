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

-- | The options of a 'LLVM.Target.TargetOptions'
-- <http://llvm.org/doxygen/classllvm_1_1TargetOptions.html>
data Options = Options {
  printMachineCode :: Bool,
  unsafeFloatingPointMath :: Bool,
  noInfinitiesFloatingPointMath :: Bool,
  noNaNsFloatingPointMath :: Bool,
  honorSignDependentRoundingFloatingPointMathOption :: Bool,
  noZerosInBSS :: Bool,
  guaranteedTailCallOptimization :: Bool,
  enableFastInstructionSelection :: Bool,
  useInitArray :: Bool,
  disableIntegratedAssembler :: Bool,
  compressDebugSections :: DebugCompressionType,
  trapUnreachable :: Bool,
  stackAlignmentOverride :: Word32,
  floatABIType :: FloatABI,
  allowFloatingPointOperationFusion :: FloatingPointOperationFusionMode
  }
  deriving (Eq, Ord, Read, Show)

