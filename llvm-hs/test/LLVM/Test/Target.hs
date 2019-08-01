{-# LANGUAGE
  CPP,
  OverloadedStrings,
  RecordWildCards,
  ScopedTypeVariables
  #-}
module LLVM.Test.Target where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Test.QuickCheck

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as ByteString
import           Data.Char
import           Data.Map (Map)
import           Data.Monoid (mempty)
import           Foreign.C.String

import qualified LLVM.CodeGenOpt as CodeGenOpt
import qualified LLVM.CodeModel as CodeModel
import           LLVM.Context
import           LLVM.Internal.Coding
import           LLVM.Internal.DecodeAST
import           LLVM.Internal.EncodeAST
import qualified LLVM.Relocation as Reloc
import           LLVM.Target
import           LLVM.Target.LibraryFunction
import           LLVM.Target.Options

instance Arbitrary FloatABI where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary FloatingPointOperationFusionMode where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary ThreadModel where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary EABIVersion where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary DebuggerKind where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary FloatingPointDenormalMode where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary ExceptionHandling where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary Options where
  arbitrary = do
    printMachineCode <- arbitrary
    unsafeFloatingPointMath <- arbitrary
    noInfinitiesFloatingPointMath <- arbitrary
    noNaNsFloatingPointMath <- arbitrary
    noTrappingFloatingPointMath <- arbitrary
    noSignedZeroesFloatingPointMath <- arbitrary
    honorSignDependentRoundingFloatingPointMathOption <- arbitrary
    noZerosInBSS <- arbitrary
    guaranteedTailCallOptimization <- arbitrary
    stackSymbolOrdering <- arbitrary
    enableFastInstructionSelection <- arbitrary
    useInitArray <- arbitrary
    disableIntegratedAssembler <- arbitrary
    compressDebugSections <- arbitrary
    relaxELFRelocations <- arbitrary
    functionSections <- arbitrary
    dataSections <- arbitrary
    uniqueSectionNames <- arbitrary
    trapUnreachable <- arbitrary
    emulatedThreadLocalStorage <- arbitrary
    enableInterProceduralRegisterAllocation <- arbitrary
    stackAlignmentOverride <- arbitrary
    floatABIType <- arbitrary
    allowFloatingPointOperationFusion <- arbitrary
    threadModel <- arbitrary
    eabiVersion <- arbitrary
    debuggerTuning <- arbitrary
    floatingPointDenormalMode <- arbitrary
    exceptionModel <- arbitrary
    machineCodeOptions <- arbitrary
    return Options { .. }

instance Arbitrary MachineCodeOptions where
  arbitrary = do
    relaxAll <- arbitrary
    noExecutableStack <- arbitrary
    fatalWarnings <- arbitrary
    noWarnings <- arbitrary
    noDeprecatedWarning <- arbitrary
    saveTemporaryLabels <- arbitrary
    useDwarfDirectory <- arbitrary
    incrementalLinkerCompatible <- arbitrary
    pieCopyRelocations <- arbitrary
    showMachineCodeEncoding <- arbitrary
    showMachineCodeInstructions <- arbitrary
    verboseAssembly <- arbitrary
    preserveComentsInAssembly <- arbitrary
    return MachineCodeOptions { .. }

instance Arbitrary DebugCompressionType where
  arbitrary = elements [CompressNone, CompressGNU, CompressZ]

arbitraryASCIIString :: Gen String
#if MIN_VERSION_QuickCheck(2,10,0)
arbitraryASCIIString = getASCIIString <$> arbitrary
#else
arbitraryASCIIString = arbitrary
#endif

instance Arbitrary CPUFeature where
  arbitrary = CPUFeature . ByteString.pack <$>
    suchThat arbitraryASCIIString (\s -> not (null s) && all isAlphaNum s)

tests = testGroup "Target" [
  testGroup "Options" [
     testProperty "basic" $ \options -> ioProperty $ do
       withTargetOptions $ \to -> do
         pokeTargetOptions options to
         options' <- peekTargetOptions to
         return $ options === options',
     testProperty "target machine" $ \options -> ioProperty $ do
         withTargetOptions $ \to -> do
           pokeTargetOptions options to
           let triple = "i386-linux-gnu"
               cpu = ""
               features = mempty
               reloc = Reloc.Default
               codeModel = CodeModel.Default
               codeGenOpt = CodeGenOpt.Default
           (target, _) <- lookupTarget Nothing triple
           withTargetMachine target triple cpu features to reloc codeModel codeGenOpt $ \tm -> do
             options' <- peekTargetOptions =<< targetMachineOptions tm
             return $ options === options'
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
   ],
  testCase "Host" $ do
    features <- getHostCPUFeatures
    return (),
  testGroup "CPUFeature" [
    testProperty "roundtrip" $ \cpuFeatures -> ioProperty $
        withContext $ \context -> runEncodeAST context $ do
          encodedFeatures :: CString <- (encodeM cpuFeatures)
          decodedFeatures :: Map CPUFeature Bool <- liftIO $ runDecodeAST (decodeM encodedFeatures)
          return (cpuFeatures == decodedFeatures)

   ]
 ]
