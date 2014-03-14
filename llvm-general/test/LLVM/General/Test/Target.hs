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
import LLVM.General.Target.LibraryFunction

instance Arbitrary FloatABI where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary FloatingPointOperationFusionMode where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary Options where
  arbitrary = do
    printMachineCode <- arbitrary
    noFramePointerElimination <- arbitrary
    lessPreciseFloatingPointMultiplyAddOption <- arbitrary
    unsafeFloatingPointMath <- arbitrary
    noInfinitiesFloatingPointMath <- arbitrary
    noNaNsFloatingPointMath <- arbitrary
    honorSignDependentRoundingFloatingPointMathOption <- arbitrary
    useSoftFloat <- arbitrary
    noZerosInBSS <- arbitrary
    jITEmitDebugInfo <- arbitrary
    jITEmitDebugInfoToDisk <- arbitrary
    guaranteedTailCallOptimization <- arbitrary
    disableTailCalls <- arbitrary
    enableFastInstructionSelection <- arbitrary
    positionIndependentExecutable <- arbitrary
    enableSegmentedStacks <- arbitrary
    useInitArray <- arbitrary
    stackAlignmentOverride <- arbitrary
    trapFunctionName <- elements [ "foo", "bar", "baz" ]
    floatABIType <- arbitrary
    allowFloatingPointOperationFusion <- arbitrary
    return Options { .. }

tests = testGroup "Target" [
  testGroup "Options" [
     testGroup "regressions" [
       testCase "hurm" $ do
         withTargetOptions $ \to -> do
           let o = Options {
                    printMachineCode = True,
                    noFramePointerElimination = False,
                    lessPreciseFloatingPointMultiplyAddOption = True,
                    unsafeFloatingPointMath = True,
                    noInfinitiesFloatingPointMath = True,
                    noNaNsFloatingPointMath = False,
                    honorSignDependentRoundingFloatingPointMathOption = True,
                    useSoftFloat = True,
                    noZerosInBSS = False,
                    jITEmitDebugInfo = True,
                    jITEmitDebugInfoToDisk = False,
                    guaranteedTailCallOptimization = False,
                    disableTailCalls = False,
                    enableFastInstructionSelection = True,
                    positionIndependentExecutable = True,
                    enableSegmentedStacks = False,
                    useInitArray = True,
                    stackAlignmentOverride = 9432851444,
                    trapFunctionName = "baz",
                    floatABIType = FloatABISoft,
                    allowFloatingPointOperationFusion = FloatingPointOperationFusionStrict
                   }
           pokeTargetOptions o to
           o' <- peekTargetOptions to
           o' @?= o
       ],
     testProperty "basic" $ \options -> morallyDubiousIOProperty $ do
       withTargetOptions $ \to -> do
         pokeTargetOptions options to
         options' <- peekTargetOptions to
         return $ options == options'
   ],
  testGroup "LibraryFunction" [
    testGroup "set-get" [
       testCase (show lf) $ do
         triple <- getDefaultTargetTriple
         withTargetLibraryInfo triple $ \tli -> do
           setLibraryFunctionAvailableWithName tli lf "foo"
           nm <- getLibraryFunctionName tli lf
           nm @?= "foo"
       | lf <- [minBound, maxBound]
     ],
    testCase "get" $ do
      triple <- getDefaultTargetTriple
      withTargetLibraryInfo triple $ \tli -> do
        lf <- getLibraryFunction tli "printf"
        lf @?= Just LF__printf
   ]
 ]
