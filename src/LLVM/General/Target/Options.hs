{-# LANGUAGE
  DeriveDataTypeable 
  #-}
module LLVM.General.Target.Options where

import Data.Data
import Data.Word

data FloatABI
  = FloatABIDefault
  | FloatABISoft
  | FloatABIHard
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Data)

data FloatingPointOperationFusionMode
  = FloatingPointOperationFusionFast
  | FloatingPointOperationFusionStandard
  | FloatingPointOperationFusionStrict
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Data)

data Options = Options {
  printMachineCode :: Bool,
  noFramePointerElimination :: Bool,
  noFramePointerEliminationNonLeaf :: Bool,
  lessPreciseFloatingPointMultiplyAddOption :: Bool,
  unsafeFloatingPointMath :: Bool,
  noInfinitiesFloatingPointMath :: Bool,
  noNaNsFloatingPointMath :: Bool,
  honorSignDependentRoundingFloatingPointMathOption :: Bool,
  useSoftFloat :: Bool,
  noZerosInBSS :: Bool,
  jITExceptionHandling :: Bool,
  jITEmitDebugInfo :: Bool,
  jITEmitDebugInfoToDisk :: Bool,
  guaranteedTailCallOptimization :: Bool,
  disableTailCalls :: Bool,
  realignStack :: Bool,
  enableFastInstructionSelection :: Bool,
  positionIndependentExecutable :: Bool,
  enableSegmentedStacks :: Bool,
  useInitArray :: Bool,
  stackAlignmentOverride :: Word32,
  trapFunctionName :: String,
  floatABIType :: FloatABI,
  allowFloatingPointOperationFusion :: FloatingPointOperationFusionMode,
  stackSmashingProtectionBufferSize :: Word32
  }
  deriving (Eq, Ord, Read, Show)

