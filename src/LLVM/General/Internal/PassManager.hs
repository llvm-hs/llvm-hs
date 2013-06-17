{-# LANGUAGE
  TemplateHaskell,
  FlexibleInstances
  #-}

module LLVM.General.Internal.PassManager where

import qualified Language.Haskell.TH as TH

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative

import Control.Monad.AnyCont

import Foreign.Ptr

import qualified LLVM.General.Internal.FFI.PassManager as FFI
import qualified LLVM.General.Internal.FFI.Transforms as FFI

import LLVM.General.Internal.Module
import LLVM.General.Internal.Target
import LLVM.General.Internal.Coding
import LLVM.General.Transforms

newtype PassManager = PassManager (Ptr FFI.PassManager)

class PassManagerSpecification s where
    createPassManager :: s -> IO (Ptr FFI.PassManager)

data CuratedPassSetSpec = CuratedPassSetSpec {
      optLevel :: Maybe Int,
      sizeLevel :: Maybe Int,
      unitAtATime :: Maybe Bool,
      simplifyLibCalls :: Maybe Bool,
      useInlinerWithThreshold :: Maybe Int
  }

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

data PassSetSpec = PassSetSpec [Pass] (Maybe TargetLowering)

instance PassManagerSpecification PassSetSpec where
  createPassManager (PassSetSpec ps tl') = flip runAnyContT return $ do
    let tl = maybe nullPtr (\(TargetLowering tl) -> tl) tl'
    pm <- liftIO $ FFI.createPassManager
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
                      ++ ["tl" | FFI.needsTargetLowering (TH.nameBase n)]
                      ++ fns)
                  )
                 |]
               ]
          TH.match (TH.conP n $ map (TH.varP . TH.mkName) fns) (TH.normalB (TH.doE actions)) []
     )
    return pm

instance PassManagerSpecification [Pass] where
    createPassManager ps = createPassManager (PassSetSpec ps Nothing)

instance PassManagerSpecification ([Pass], TargetLowering) where
    createPassManager (ps, tl) = createPassManager (PassSetSpec ps (Just tl))

withPassManager :: PassManagerSpecification s => s -> (PassManager -> IO a) -> IO a
withPassManager s = bracket (createPassManager s) FFI.disposePassManager . (. PassManager)

runPassManager :: PassManager -> Module -> IO Bool
runPassManager (PassManager p) (Module m) = toEnum . fromIntegral <$> FFI.runPassManager p m
