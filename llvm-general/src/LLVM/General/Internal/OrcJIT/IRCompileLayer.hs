{-# LANGUAGE MultiParamTypeClasses #-}
module LLVM.General.Internal.OrcJIT.IRCompileLayer where

import LLVM.General.Prelude

import Control.Exception
import Control.Monad.AnyCont
import Control.Monad.IO.Class
import Data.IORef
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr

import LLVM.General.Internal.Coding
import qualified LLVM.General.Internal.FFI.DataLayout as FFI
import qualified LLVM.General.Internal.FFI.OrcJIT as FFI
import qualified LLVM.General.Internal.FFI.OrcJIT.IRCompileLayer as FFI
import qualified LLVM.General.Internal.FFI.Target as FFI
import LLVM.General.Internal.Module
import LLVM.General.Internal.OrcJIT
import LLVM.General.Internal.Target

data IRCompileLayer =
  IRCompileLayer {
    compileLayer :: !(Ptr FFI.IRCompileLayer),
    dataLayout :: !(Ptr FFI.DataLayout),
    cleanupActions :: !(IORef [IO ()])
  }
  deriving Eq

newtype ModuleSet = ModuleSet (Ptr FFI.ModuleSetHandle)

withIRCompileLayer :: ObjectLinkingLayer -> TargetMachine -> (IRCompileLayer -> IO a) -> IO a
withIRCompileLayer (ObjectLinkingLayer oll) (TargetMachine tm) f = flip runAnyContT return $ do
  dl <- anyContToM $ bracket (FFI.createTargetDataLayout tm) FFI.disposeDataLayout
  cl <- anyContToM $ bracket (FFI.createIRCompileLayer oll tm) FFI.disposeIRCompileLayer
  cleanup <- anyContToM $ bracket (newIORef []) (sequence <=< readIORef)
  liftIO $ f (IRCompileLayer cl dl cleanup)

mangleSymbol :: IRCompileLayer -> String -> IO MangledSymbol
mangleSymbol (IRCompileLayer _ dl _) symbol = flip runAnyContT return $ do
  mangledSymbol <- alloca
  symbol' <- encodeM symbol
  anyContToM $ bracket
    (FFI.getMangledSymbol mangledSymbol symbol' dl)
    (\_ -> FFI.disposeMangledSymbol =<< peek mangledSymbol)
  decodeM =<< peek mangledSymbol

findSymbol :: IRCompileLayer -> MangledSymbol -> Bool -> IO JITSymbol
findSymbol (IRCompileLayer cl _ _) symbol exportedSymbolsOnly = flip runAnyContT return $ do
  symbol' <- encodeM symbol
  exportedSymbolsOnly' <- encodeM exportedSymbolsOnly
  symbol <- anyContToM $ bracket
    (FFI.findSymbol cl symbol' exportedSymbolsOnly') FFI.disposeSymbol
  decodeM symbol

addModuleSet :: IRCompileLayer -> [Module] -> SymbolResolver -> IO ModuleSet
addModuleSet (IRCompileLayer cl dl cleanups) modules resolver = flip runAnyContT return $ do
  resolverAct <- encodeM resolver
  resolver' <- liftIO $ resolverAct cleanups
  modules' <- liftIO $ mapM readModule modules
  (moduleCount, modules'') <-
    anyContToM $ \f -> withArrayLen modules' $ \n hs -> f (fromIntegral n, hs)
  moduleSet <- liftIO $ FFI.addModuleSet cl dl modules'' moduleCount resolver'
  pure (ModuleSet moduleSet)

removeModuleSet :: IRCompileLayer -> ModuleSet -> IO ()
removeModuleSet (IRCompileLayer cl _ _) (ModuleSet handle) =
  FFI.removeModuleSet cl handle

withModuleSet :: IRCompileLayer -> [Module] -> SymbolResolver -> (ModuleSet -> IO a) -> IO a
withModuleSet compileLayer modules resolver =
  bracket
    (addModuleSet compileLayer modules resolver)
    (removeModuleSet compileLayer)
