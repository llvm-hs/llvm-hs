{-# LANGUAGE
  RecordWildCards
  #-}
module LLVM.General.Test.Target where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Property

import Control.Monad

import LLVM.General.Target
import LLVM.General.Target.Options

instance Arbitrary FloatABI where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary FloatingPointOperationFusionMode where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary Options where
  arbitrary = do
    printMachineCode <- arbitrary
    noFramePointerElimination <- arbitrary
    noFramePointerEliminationNonLeaf <- arbitrary
    lessPreciseFloatingPointMultiplyAddOption <- arbitrary
    unsafeFloatingPointMath <- arbitrary
    noInfinitiesFloatingPointMath <- arbitrary
    noNaNsFloatingPointMath <- arbitrary
    honorSignDependentRoundingFloatingPointMathOption <- arbitrary
    useSoftFloat <- arbitrary
    noZerosInBSS <- arbitrary
    jITExceptionHandling <- arbitrary
    jITEmitDebugInfo <- arbitrary
    jITEmitDebugInfoToDisk <- arbitrary
    guaranteedTailCallOptimization <- arbitrary
    disableTailCalls <- arbitrary
    realignStack <- arbitrary
    enableFastInstructionSelection <- arbitrary
    positionIndependentExecutable <- arbitrary
    enableSegmentedStacks <- arbitrary
    useInitArray <- arbitrary
    stackAlignmentOverride <- arbitrary
    trapFunctionName <- elements [ "foo", "bar", "baz" ]
    floatABIType <- arbitrary
    allowFloatingPointOperationFusion <- arbitrary
    stackSmashingProtectionBufferSize <- arbitrary
    return Options { .. }

tests = testGroup "Target" [
  testGroup "Options" [
     testGroup "regressions" [
       testCase "hurm" $ do
         withTargetOptions $ \to -> do
           let o = Options {printMachineCode = True, noFramePointerElimination = False, noFramePointerEliminationNonLeaf = True, lessPreciseFloatingPointMultiplyAddOption = True, unsafeFloatingPointMath = True, noInfinitiesFloatingPointMath = True, noNaNsFloatingPointMath = False, honorSignDependentRoundingFloatingPointMathOption = True, useSoftFloat = True, noZerosInBSS = False, jITExceptionHandling = True, jITEmitDebugInfo = True, jITEmitDebugInfoToDisk = False, guaranteedTailCallOptimization = False, disableTailCalls = False, realignStack = False, enableFastInstructionSelection = True, positionIndependentExecutable = True, enableSegmentedStacks = False, useInitArray = True, stackAlignmentOverride = 9432851444, trapFunctionName = "baz", floatABIType = FloatABISoft, allowFloatingPointOperationFusion = FloatingPointOperationFusionStrict, stackSmashingProtectionBufferSize = 2650013862}
           pokeTargetOptions o to
           o' <- peekTargetOptions to
           o' @?= o
       ],
     testProperty "basic" $ \options -> morallyDubiousIOProperty $ do
       withTargetOptions $ \to -> do
         pokeTargetOptions options to
         options' <- peekTargetOptions to
         return $ options == options'
   ]
 ]
