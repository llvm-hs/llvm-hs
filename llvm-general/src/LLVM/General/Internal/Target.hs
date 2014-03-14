{-# LANGUAGE
  TemplateHaskell,
  MultiParamTypeClasses,
  RecordWildCards,
  UndecidableInstances
  #-}
module LLVM.General.Internal.Target where

import Control.Monad hiding (forM)
import Control.Monad.Error hiding (forM)
import Control.Exception
import Data.Functor
import Data.Traversable (forM)
import Control.Monad.AnyCont
import Data.Maybe

import Foreign.Ptr
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set

import LLVM.General.Internal.Coding
import LLVM.General.Internal.String ()
import LLVM.General.Internal.LibraryFunction
import LLVM.General.DataLayout

import LLVM.General.AST.DataLayout

import qualified LLVM.General.Internal.FFI.LLVMCTypes as FFI
import qualified LLVM.General.Internal.FFI.Target as FFI

import qualified LLVM.General.Relocation as Reloc
import qualified LLVM.General.Target.Options as TO
import qualified LLVM.General.CodeModel as CodeModel
import qualified LLVM.General.CodeGenOpt as CodeGenOpt

genCodingInstance [t| Reloc.Model |] ''FFI.RelocModel [
  (FFI.relocModelDefault, Reloc.Default),
  (FFI.relocModelStatic, Reloc.Static),
  (FFI.relocModelPIC, Reloc.PIC),
  (FFI.relocModelDynamicNoPic, Reloc.DynamicNoPIC)
 ]

genCodingInstance [t| CodeModel.Model |] ''FFI.CodeModel [
  (FFI.codeModelDefault,CodeModel.Default),
  (FFI.codeModelJITDefault, CodeModel.JITDefault),
  (FFI.codeModelSmall, CodeModel.Small),
  (FFI.codeModelKernel, CodeModel.Kernel),
  (FFI.codeModelMedium, CodeModel.Medium),
  (FFI.codeModelLarge, CodeModel.Large)
 ]

genCodingInstance [t| CodeGenOpt.Level |] ''FFI.CodeGenOptLevel [
  (FFI.codeGenOptLevelNone, CodeGenOpt.None),
  (FFI.codeGenOptLevelLess, CodeGenOpt.Less),
  (FFI.codeGenOptLevelDefault, CodeGenOpt.Default),
  (FFI.codeGenOptLevelAggressive, CodeGenOpt.Aggressive)
 ]

genCodingInstance [t| TO.FloatABI |] ''FFI.FloatABIType [
  (FFI.floatABIDefault, TO.FloatABIDefault),
  (FFI.floatABISoft, TO.FloatABISoft),
  (FFI.floatABIHard, TO.FloatABIHard)
 ]

genCodingInstance [t| TO.FloatingPointOperationFusionMode |] ''FFI.FPOpFusionMode [
  (FFI.fpOpFusionModeFast, TO.FloatingPointOperationFusionFast),
  (FFI.fpOpFusionModeStandard, TO.FloatingPointOperationFusionStandard),
  (FFI.fpOpFusionModeStrict, TO.FloatingPointOperationFusionStrict)
 ]

-- | <http://llvm.org/doxygen/classllvm_1_1Target.html>
newtype Target = Target (Ptr FFI.Target)

-- | e.g. an instruction set extension
newtype CPUFeature = CPUFeature String
  deriving (Eq, Ord, Read, Show)

instance EncodeM e String es => EncodeM e (Set CPUFeature) es where
  encodeM = encodeM . intercalate " " . map (\(CPUFeature f) -> f) . Set.toList

instance (Monad d, DecodeM d String es) => DecodeM d (Set CPUFeature) es where
  decodeM = liftM (Set.fromList . map CPUFeature . words) . decodeM

-- | Find a 'Target' given an architecture and/or a \"triple\".
-- | <http://llvm.org/doxygen/structllvm_1_1TargetRegistry.html#a3105b45e546c9cc3cf78d0f2ec18ad89>
-- | Be sure to run either 'initializeAllTargets' or 'initializeNativeTarget' before expecting this to succeed, depending on what target(s) you want to use.
lookupTarget :: 
  Maybe String -- ^ arch
  -> String -- ^ \"triple\" - e.g. x86_64-unknown-linux-gnu
  -> ErrorT String IO (Target, String)
lookupTarget arch triple = flip runAnyContT return $ do
  cErrorP <- alloca
  cNewTripleP <- alloca
  arch <- encodeM (maybe "" id arch)
  triple <- encodeM triple
  target <- liftIO $ FFI.lookupTarget arch triple cNewTripleP cErrorP
  when (target == nullPtr) $ fail =<< decodeM cErrorP
  liftM (Target target, ) $ decodeM cNewTripleP

-- | <http://llvm.org/doxygen/classllvm_1_1TargetOptions.html>
newtype TargetOptions = TargetOptions (Ptr FFI.TargetOptions)

-- | bracket creation and destruction of a 'TargetOptions' object
withTargetOptions :: (TargetOptions -> IO a) -> IO a
withTargetOptions = bracket FFI.createTargetOptions FFI.disposeTargetOptions . (. TargetOptions)

-- | set all target options
pokeTargetOptions :: TO.Options -> TargetOptions -> IO ()
pokeTargetOptions hOpts (TargetOptions cOpts) = do
  mapM_ (\(c, ha) -> FFI.setTargetOptionFlag cOpts c =<< encodeM (ha hOpts)) [
    (FFI.targetOptionFlagPrintMachineCode, TO.printMachineCode),
    (FFI.targetOptionFlagNoFramePointerElim, TO.noFramePointerElimination),
    (FFI.targetOptionFlagLessPreciseFPMADOption, TO.lessPreciseFloatingPointMultiplyAddOption),
    (FFI.targetOptionFlagUnsafeFPMath, TO.unsafeFloatingPointMath),
    (FFI.targetOptionFlagNoInfsFPMath, TO.noInfinitiesFloatingPointMath),
    (FFI.targetOptionFlagNoNaNsFPMath, TO.noNaNsFloatingPointMath),
    (FFI.targetOptionFlagHonorSignDependentRoundingFPMathOption, TO.honorSignDependentRoundingFloatingPointMathOption),
    (FFI.targetOptionFlagUseSoftFloat, TO.useSoftFloat),
    (FFI.targetOptionFlagNoZerosInBSS, TO.noZerosInBSS),
    (FFI.targetOptionFlagJITEmitDebugInfo, TO.jITEmitDebugInfo),
    (FFI.targetOptionFlagJITEmitDebugInfoToDisk, TO.jITEmitDebugInfoToDisk),
    (FFI.targetOptionFlagGuaranteedTailCallOpt, TO.guaranteedTailCallOptimization),
    (FFI.targetOptionFlagDisableTailCalls, TO.disableTailCalls),
    (FFI.targetOptionFlagEnableFastISel, TO.enableFastInstructionSelection),
    (FFI.targetOptionFlagPositionIndependentExecutable, TO.positionIndependentExecutable),
    (FFI.targetOptionFlagEnableSegmentedStacks, TO.enableSegmentedStacks),
    (FFI.targetOptionFlagUseInitArray, TO.useInitArray)
   ]
  FFI.setStackAlignmentOverride cOpts =<< encodeM (TO.stackAlignmentOverride hOpts)
  flip runAnyContT return $ do
    n <- encodeM (TO.trapFunctionName hOpts)
    liftIO $ FFI.setTrapFuncName cOpts n
  FFI.setFloatABIType cOpts =<< encodeM (TO.floatABIType hOpts)
  FFI.setAllowFPOpFusion cOpts =<< encodeM (TO.allowFloatingPointOperationFusion hOpts)
  
-- | get all target options
peekTargetOptions :: TargetOptions -> IO TO.Options
peekTargetOptions (TargetOptions tOpts) = do
  let gof = decodeM <=< FFI.getTargetOptionsFlag tOpts 
  printMachineCode
    <- gof FFI.targetOptionFlagPrintMachineCode
  noFramePointerElimination
    <- gof FFI.targetOptionFlagNoFramePointerElim
  lessPreciseFloatingPointMultiplyAddOption
    <- gof FFI.targetOptionFlagLessPreciseFPMADOption
  unsafeFloatingPointMath
    <- gof FFI.targetOptionFlagUnsafeFPMath
  noInfinitiesFloatingPointMath
    <- gof FFI.targetOptionFlagNoInfsFPMath
  noNaNsFloatingPointMath
    <- gof FFI.targetOptionFlagNoNaNsFPMath
  honorSignDependentRoundingFloatingPointMathOption
    <- gof FFI.targetOptionFlagHonorSignDependentRoundingFPMathOption
  useSoftFloat
    <- gof FFI.targetOptionFlagUseSoftFloat
  noZerosInBSS
    <- gof FFI.targetOptionFlagNoZerosInBSS
  jITEmitDebugInfo
    <- gof FFI.targetOptionFlagJITEmitDebugInfo
  jITEmitDebugInfoToDisk
    <- gof FFI.targetOptionFlagJITEmitDebugInfoToDisk
  guaranteedTailCallOptimization
    <- gof FFI.targetOptionFlagGuaranteedTailCallOpt
  disableTailCalls
    <- gof FFI.targetOptionFlagDisableTailCalls
  enableFastInstructionSelection
    <- gof FFI.targetOptionFlagEnableFastISel
  positionIndependentExecutable
    <- gof FFI.targetOptionFlagPositionIndependentExecutable
  enableSegmentedStacks
    <- gof FFI.targetOptionFlagEnableSegmentedStacks
  useInitArray
    <- gof FFI.targetOptionFlagUseInitArray
  stackAlignmentOverride <- decodeM =<< FFI.getStackAlignmentOverride tOpts
  trapFunctionName <- decodeM =<< FFI.getTrapFuncName tOpts
  floatABIType <- decodeM =<< FFI.getFloatABIType tOpts
  allowFloatingPointOperationFusion <- decodeM =<< FFI.getAllowFPOpFusion tOpts
  return TO.Options { .. }

-- | <http://llvm.org/doxygen/classllvm_1_1TargetMachine.html>
newtype TargetMachine = TargetMachine (Ptr FFI.TargetMachine)

-- | bracket creation and destruction of a 'TargetMachine'
withTargetMachine :: 
    Target
    -> String -- ^ triple
    -> String -- ^ cpu
    -> Set CPUFeature -- ^ features
    -> TargetOptions
    -> Reloc.Model
    -> CodeModel.Model
    -> CodeGenOpt.Level
    -> (TargetMachine -> IO a)
    -> IO a
withTargetMachine
  (Target target)
  triple
  cpu
  features
  (TargetOptions targetOptions)
  relocModel
  codeModel
  codeGenOptLevel = runAnyContT $ do
  triple <- encodeM triple
  cpu <- encodeM cpu
  features <- encodeM features
  relocModel <- encodeM relocModel
  codeModel <- encodeM codeModel
  codeGenOptLevel <- encodeM codeGenOptLevel
  anyContToM $ bracket (
      FFI.createTargetMachine
         target
         triple
         cpu
         features
         targetOptions
         relocModel
         codeModel
         codeGenOptLevel
      )
      FFI.disposeTargetMachine
      . (. TargetMachine)

-- | <http://llvm.org/doxygen/classllvm_1_1TargetLowering.html>
newtype TargetLowering = TargetLowering (Ptr FFI.TargetLowering)

-- | get the 'TargetLowering' of a 'TargetMachine'
getTargetLowering :: TargetMachine -> IO TargetLowering
getTargetLowering (TargetMachine tm) = TargetLowering <$> FFI.getTargetLowering tm

-- | Initialize the native target. This function is called automatically in these Haskell bindings
-- when creating an 'LLVM.General.ExecutionEngine.ExecutionEngine' which will require it, and so it should
-- not be necessary to call it separately.
initializeNativeTarget :: IO ()
initializeNativeTarget = do
  failure <- decodeM =<< liftIO FFI.initializeNativeTarget
  when failure $ fail "native target initialization failed"

-- | the default target triple that LLVM has been configured to produce code for
getDefaultTargetTriple :: IO String
getDefaultTargetTriple = decodeM =<< FFI.getDefaultTargetTriple

-- | a target triple suitable for loading code into the current process
getProcessTargetTriple :: IO String
getProcessTargetTriple = decodeM =<< FFI.getProcessTargetTriple

-- | the LLVM name for the host CPU
getHostCPUName :: IO String
getHostCPUName = decodeM =<< FFI.getHostCPUName

-- | a space-separated list of LLVM feature names supported by the host CPU
getHostCPUFeatures :: IO (Set CPUFeature)
getHostCPUFeatures = decodeM =<< FFI.getHostCPUFeatures
  
-- | 'DataLayout' to use for the given 'TargetMachine'
getTargetMachineDataLayout :: TargetMachine -> IO DataLayout
getTargetMachineDataLayout (TargetMachine m) =
    fromMaybe (error "parseDataLayout failed") . parseDataLayout <$> (decodeM =<< (FFI.getTargetMachineDataLayout m))

-- | Initialize all targets so they can be found by 'lookupTarget'
initializeAllTargets :: IO ()
initializeAllTargets = FFI.initializeAllTargets

-- | Bracket creation and destruction of a 'TargetMachine' configured for the host
withDefaultTargetMachine :: (TargetMachine -> IO a) -> ErrorT String IO a
withDefaultTargetMachine f = do
  liftIO $ initializeAllTargets
  triple <- liftIO $ getDefaultTargetTriple
  cpu <- liftIO $ getHostCPUName
  features <- liftIO $ getHostCPUFeatures
  (target, _) <- lookupTarget Nothing triple
  liftIO $ withTargetOptions $ \options ->
      withTargetMachine target triple cpu features options Reloc.Default CodeModel.Default CodeGenOpt.Default f

-- | <http://llvm.org/docs/doxygen/html/classllvm_1_1TargetLibraryInfo.html>
newtype TargetLibraryInfo = TargetLibraryInfo (Ptr FFI.TargetLibraryInfo)

-- | Look up a 'LibraryFunction' by its standard name
getLibraryFunction :: TargetLibraryInfo -> String -> IO (Maybe LibraryFunction)
getLibraryFunction (TargetLibraryInfo f) name = flip runAnyContT return $ do
  libFuncP <- alloca
  name <- encodeM name
  r <- decodeM =<< (liftIO $ FFI.getLibFunc f name libFuncP)
  forM (if r then Just libFuncP else Nothing) $ decodeM <=< peek

-- | Get a the current name to be emitted for a 'LibraryFunction'
getLibraryFunctionName :: TargetLibraryInfo -> LibraryFunction -> IO String
getLibraryFunctionName (TargetLibraryInfo f) l = flip runAnyContT return $ do
  l <- encodeM l
  decodeM $ FFI.libFuncGetName f l

-- | Set the name of the function on the target platform that corresponds to funcName
setLibraryFunctionAvailableWithName ::
  TargetLibraryInfo
  -> LibraryFunction
  -> String -- ^ The function name to be emitted
  -> IO ()
setLibraryFunctionAvailableWithName (TargetLibraryInfo f) libraryFunction name = flip runAnyContT return $ do
  name <- encodeM name
  libraryFunction <- encodeM libraryFunction
  liftIO $ FFI.libFuncSetAvailableWithName f libraryFunction name

-- | look up information about the library functions available on a given platform
withTargetLibraryInfo :: 
  String -- ^ triple
  -> (TargetLibraryInfo -> IO a)
  -> IO a
withTargetLibraryInfo triple f = flip runAnyContT return $ do
  triple <- encodeM triple
  liftIO $ bracket (FFI.createTargetLibraryInfo triple) FFI.disposeTargetLibraryInfo (f . TargetLibraryInfo)
