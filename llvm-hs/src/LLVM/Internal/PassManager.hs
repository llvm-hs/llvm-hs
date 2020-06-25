{-# LANGUAGE
  TemplateHaskell,
  MultiParamTypeClasses,
  CPP
  #-}
module LLVM.Internal.PassManager where

import LLVM.Prelude

import qualified Language.Haskell.TH as TH

import Control.Monad.AnyCont
import Control.Monad.Catch
import Control.Monad.IO.Class

import qualified Data.ByteString.Short as ByteString

import Foreign.C (CString)
import Foreign.Ptr
import GHC.Stack

import qualified LLVM.Internal.FFI.PassManager as FFI
import qualified LLVM.Internal.FFI.Transforms as FFI

import LLVM.Exception
import LLVM.Internal.Module
import LLVM.Internal.Target
import LLVM.Internal.Coding
import LLVM.Transforms

import LLVM.AST.DataLayout

-- | <http://llvm.org/doxygen/classllvm_1_1PassManager.html>
-- Note: a PassManager does substantive behind-the-scenes work, arranging for the
-- results of various analyses to be available as needed by transform passes, shared
-- as possible.
newtype PassManager = PassManager (Ptr FFI.PassManager)

-- | There are different ways to get a 'PassManager'. This type embodies them.
data PassSetSpec
  -- | a 'PassSetSpec' is a lower-level, detailed specification of a set of passes. It
  -- allows fine-grained control of what passes are to be run when, and the specification
  -- of passes not available through 'CuratedPassSetSpec'.
  = PassSetSpec {
      transforms :: [Pass],
      dataLayout :: Maybe DataLayout,
      targetLibraryInfo :: Maybe TargetLibraryInfo,
      targetMachine :: Maybe TargetMachine
    }
  -- | This type is a high-level specification of a set of passes. It uses the same
  -- collection of passes chosen by the LLVM team in the command line tool 'opt'.  The fields
  -- of this spec are much like typical compiler command-line flags - e.g. -O\<n\>, etc.
  | CuratedPassSetSpec {
      optLevel :: Maybe Word,
      sizeLevel :: Maybe Word,
      unitAtATime :: Maybe Bool,
      simplifyLibCalls :: Maybe Bool,
      loopVectorize :: Maybe Bool,
      superwordLevelParallelismVectorize :: Maybe Bool,
      useInlinerWithThreshold :: Maybe Word,
      dataLayout :: Maybe DataLayout,
      targetLibraryInfo :: Maybe TargetLibraryInfo,
      targetMachine :: Maybe TargetMachine
    }

-- | Helper to make a curated 'PassSetSpec'
defaultCuratedPassSetSpec :: PassSetSpec
defaultCuratedPassSetSpec = CuratedPassSetSpec {
  optLevel = Nothing,
  sizeLevel = Nothing,
  unitAtATime = Nothing,
  simplifyLibCalls = Nothing,
  loopVectorize = Nothing,
  superwordLevelParallelismVectorize = Nothing,
  useInlinerWithThreshold = Nothing,
  dataLayout = Nothing,
  targetLibraryInfo = Nothing,
  targetMachine = Nothing
}

-- | an empty 'PassSetSpec'
defaultPassSetSpec :: PassSetSpec
defaultPassSetSpec = PassSetSpec {
  transforms = [],
  dataLayout = Nothing,
  targetLibraryInfo = Nothing,
  targetMachine = Nothing
}

instance (Monad m, MonadThrow m, MonadAnyCont IO m) => EncodeM m GCOVVersion CString where
  encodeM (GCOVVersion cs)
    | ByteString.length cs == 4 = encodeM cs
    | otherwise = throwM (EncodeException "GCOVVersion should consist of exactly 4 characters")

createPassManager :: HasCallStack => PassSetSpec -> IO (Ptr FFI.PassManager)
createPassManager pss = flip runAnyContT return $ do
  pm <- liftIO $ FFI.createPassManager
  forM_ (targetLibraryInfo pss) $ \(TargetLibraryInfo tli) -> do
    liftIO $ FFI.addTargetLibraryInfoPass pm tli
  forM_ (targetMachine pss) $ \(TargetMachine tm) -> liftIO $ FFI.addAnalysisPasses tm pm
  case pss of
    s@CuratedPassSetSpec {} -> liftIO $ do
      bracket FFI.passManagerBuilderCreate FFI.passManagerBuilderDispose $ \b -> do
        let handleOption g m = forM_ (m s) (g b <=< encodeM) 
        handleOption FFI.passManagerBuilderSetOptLevel optLevel
        handleOption FFI.passManagerBuilderSetSizeLevel sizeLevel
        handleOption FFI.passManagerBuilderSetDisableUnitAtATime (liftM not . unitAtATime)
        handleOption FFI.passManagerBuilderSetDisableSimplifyLibCalls (liftM not . simplifyLibCalls)
        handleOption FFI.passManagerBuilderUseInlinerWithThreshold useInlinerWithThreshold
        handleOption FFI.passManagerBuilderSetLoopVectorize loopVectorize
        handleOption FFI.passManagerBuilderSetSuperwordLevelParallelismVectorize superwordLevelParallelismVectorize
        FFI.passManagerBuilderPopulateModulePassManager b pm
    PassSetSpec ps _ _ tm' -> do
      let tm = maybe nullPtr (\(TargetMachine tm) -> tm) tm'
      forM_ ps $ \p -> $(
        do
#if __GLASGOW_HASKELL__ < 800
          TH.TyConI (TH.DataD _ _ _ cons _) <- TH.reify ''Pass
#else
          TH.TyConI (TH.DataD _ _ _ _ cons _) <- TH.reify ''Pass
#endif
          TH.caseE [| p |] $ flip map cons $ \con -> do
            let
              (n, fns) = case con of
                            TH.RecC n fs -> (n, [ TH.nameBase fn | (fn, _, _) <- fs ])
                            TH.NormalC n [] -> (n, [])
                            _ -> error "pass descriptor constructors with fields need to be records"
              actions = 
                [ TH.bindS (TH.varP . TH.mkName $ fn) [| encodeM $(TH.dyn fn) |] | fn <- fns ]
                ++ [
                 TH.noBindS [|
                   liftIO $(
                     foldl1 TH.appE
                     (map TH.dyn $
                        ["FFI.add" ++ TH.nameBase n ++ "Pass", "pm"]
                        ++ ["tm" | FFI.needsTargetMachine (TH.nameBase n)]
                        ++ fns)
                    )
                   |]
                 ]
            TH.match (TH.conP n $ map (TH.varP . TH.mkName) fns) (TH.normalB (TH.doE actions)) []
       )
  return pm

-- | bracket the creation of a 'PassManager'
withPassManager :: PassSetSpec -> (PassManager -> IO a) -> IO a
withPassManager s = bracket (createPassManager s) FFI.disposePassManager . (. PassManager)

-- | run the passes in a 'PassManager' on a 'Module', modifying the 'Module'.
runPassManager :: PassManager -> Module -> IO Bool
runPassManager (PassManager p) m = do
  m' <- readModule m
  toEnum . fromIntegral <$> FFI.runPassManager p m'
