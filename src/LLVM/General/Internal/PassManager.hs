{-# LANGUAGE
  TemplateHaskell,
  MultiParamTypeClasses
  #-}
module LLVM.General.Internal.PassManager where

import qualified Language.Haskell.TH as TH

import Control.Exception
import Control.Monad hiding (forM_)
import Control.Monad.IO.Class
import Control.Applicative

import Control.Monad.AnyCont

import Data.Foldable (forM_)
import Data.Word (Word)
import Foreign.C (CString)
import Foreign.Ptr

import qualified LLVM.General.Internal.FFI.PassManager as FFI
import qualified LLVM.General.Internal.FFI.Transforms as FFI
import qualified LLVM.General.Internal.FFI.LLVMCTypes as FFI

import LLVM.General.Internal.Module
import LLVM.General.Internal.Target
import LLVM.General.Internal.Coding
import LLVM.General.Internal.DataLayout
import LLVM.General.Transforms

import LLVM.General.AST.DataLayout

-- | <http://llvm.org/doxygen/classllvm_1_1PassManager.html>
newtype PassManager = PassManager (Ptr FFI.PassManager)

-- | There are different ways to get a 'PassManager'. This class embodies them.
class PassManagerSpecification s where
    -- | make a 'PassManager'
    createPassManager :: s -> IO (Ptr FFI.PassManager)

-- | This type is a high-level specification of a set of passes. It uses the same
-- collection of passes chosen by the LLVM team in the command line tool 'opt'.  The fields
-- of this spec are much like typical compiler command-line flags - e.g. -O\<n\>, etc.
data CuratedPassSetSpec = CuratedPassSetSpec {
    optLevel :: Maybe Int,
    sizeLevel :: Maybe Int,
    unitAtATime :: Maybe Bool,
    simplifyLibCalls :: Maybe Bool,
    useInlinerWithThreshold :: Maybe Int
  }

-- | Helper to make a 'CuratedPassSetSpec'
defaultCuratedPassSetSpec = CuratedPassSetSpec {
    optLevel = Nothing,
    sizeLevel = Nothing,
    unitAtATime = Nothing,
    simplifyLibCalls = Nothing,
    useInlinerWithThreshold = Nothing
  }

instance PassManagerSpecification CuratedPassSetSpec where
  createPassManager s = bracket FFI.passManagerBuilderCreate FFI.passManagerBuilderDispose $ \b -> do
    let handleOption g m = maybe (return ()) (g b . fromIntegral . fromEnum) (m s)
    handleOption FFI.passManagerBuilderSetOptLevel optLevel
    handleOption FFI.passManagerBuilderSetSizeLevel sizeLevel
    handleOption FFI.passManagerBuilderSetDisableUnitAtATime (liftM not . unitAtATime)
    handleOption FFI.passManagerBuilderSetDisableSimplifyLibCalls (liftM not . simplifyLibCalls)
    handleOption FFI.passManagerBuilderUseInlinerWithThreshold useInlinerWithThreshold
    pm <- FFI.createPassManager
    FFI.passManagerBuilderPopulateModulePassManager b pm
    return pm

data PassSetSpec = PassSetSpec [Pass] (Maybe TargetMachine) (Maybe DataLayout)

instance Monad m => EncodeM m (Maybe Bool) (FFI.NothingAsMinusOne Bool) where
  encodeM = return . FFI.NothingAsMinusOne . maybe (-1) (fromIntegral . fromEnum)

instance Monad m => EncodeM m (Maybe Word) (FFI.NothingAsMinusOne Word) where
  encodeM = return . FFI.NothingAsMinusOne . maybe (-1) fromIntegral

instance (Monad m, MonadAnyCont IO m) => EncodeM m GCOVVersion CString where
  encodeM (GCOVVersion cs@[_,_,_,_]) = encodeM cs

instance PassManagerSpecification PassSetSpec where
  createPassManager (PassSetSpec ps tm' dl) = flip runAnyContT return $ do
    let tm = maybe nullPtr (\(TargetMachine tm) -> tm) tm'
    pm <- liftIO $ FFI.createPassManager
    forM_ dl $ \dl -> do
      dl <- encodeM (dataLayoutToString dl)
      liftIO $ FFI.addDataLayoutPass pm dl
    forM ps $ \p -> $(
      do
        TH.TyConI (TH.DataD _ _ _ cons _) <- TH.reify ''Pass
        TH.caseE [| p |] $ flip map cons $ \con -> do
          let
            (n, fns) = case con of
                          TH.RecC n fs -> (n, [ TH.nameBase fn | (fn, _, _) <- fs ])
                          TH.NormalC n [] -> (n, [])
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

instance PassManagerSpecification [Pass] where
  createPassManager ps = createPassManager (PassSetSpec ps Nothing Nothing)

instance PassManagerSpecification ([Pass], TargetMachine) where
  createPassManager (ps, tm) = createPassManager (PassSetSpec ps (Just tm) Nothing)

instance PassManagerSpecification ([Pass], DataLayout) where
  createPassManager (ps, dl) = createPassManager (PassSetSpec ps Nothing (Just dl))

-- | bracket the creation of a 'PassManager'
withPassManager :: PassManagerSpecification s => s -> (PassManager -> IO a) -> IO a
withPassManager s = bracket (createPassManager s) FFI.disposePassManager . (. PassManager)

-- | run the passes in a 'PassManager' on a 'Module', modifying the 'Module'.
runPassManager :: PassManager -> Module -> IO Bool
runPassManager (PassManager p) (Module m) = toEnum . fromIntegral <$> FFI.runPassManager p m
