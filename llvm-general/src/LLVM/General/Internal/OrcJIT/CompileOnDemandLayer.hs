{-# LANGUAGE MultiParamTypeClasses #-}
module LLVM.General.Internal.OrcJIT.CompileOnDemandLayer where

import LLVM.General.Prelude

import Control.Exception
import Control.Monad.AnyCont
import Control.Monad.IO.Class
import Data.IORef
import Foreign.Marshal.Array
import Foreign.Ptr

import LLVM.General.Internal.Coding
import LLVM.General.Internal.Module
import LLVM.General.Internal.OrcJIT
import LLVM.General.Internal.OrcJIT.IRCompileLayer (IRCompileLayer(..))
import qualified LLVM.General.Internal.OrcJIT.IRCompileLayer as IRCompileLayer
import qualified LLVM.General.Internal.FFI.OrcJIT as FFI
import qualified LLVM.General.Internal.FFI.OrcJIT.CompileOnDemandLayer as FFI
import qualified LLVM.General.Internal.FFI.PtrHierarchy as FFI

type PartitioningFn = Ptr FFI.Function -> IO [Ptr FFI.Function]

newtype JITCompileCallbackManager =
  CallbackMgr (Ptr FFI.JITCompileCallbackManager)

newtype IndirectStubsManagerBuilder =
  StubsMgr (Ptr FFI.IndirectStubsManagerBuilder)

data CompileOnDemandLayer =
  CompileOnDemandLayer {
    compileLayer :: !(Ptr FFI.CompileOnDemandLayer),
    baseLayer :: !IRCompileLayer,
    cleanupActions :: !(IORef [IO ()])
  }
  deriving Eq

newtype ModuleSet = ModuleSet (Ptr FFI.ModuleSetHandle)

instance MonadIO m =>
  EncodeM m PartitioningFn (IORef [IO ()] -> IO (FunPtr FFI.PartitioningFn)) where
  encodeM partition = return $ \cleanups -> do
    allocFunPtr
      cleanups
      (FFI.wrapPartitioningFn
         (\f set -> do
           fs <- partition f
           traverse_ (FFI.insertFun set) fs
           return ()))

instance (MonadIO m, MonadAnyCont IO m) =>
  EncodeM m (Maybe (IO ())) FFI.TargetAddress where
  encodeM Nothing = return $ FFI.TargetAddress 0
  encodeM (Just f) = do
    f' <- anyContToM $ bracket (FFI.wrapErrorHandler f) freeHaskellFunPtr
    return . FFI.TargetAddress . fromIntegral . ptrToWordPtr . castFunPtrToPtr $ f'

withIndirectStubsManagerBuilder ::
  String {- ^ triple -} ->
  (IndirectStubsManagerBuilder -> IO a) ->
  IO a
withIndirectStubsManagerBuilder triple f = flip runAnyContT return $ do
  triple' <- encodeM triple
  stubsMgr <- anyContToM $ bracket
    (FFI.createLocalIndirectStubsManagerBuilder triple')
    FFI.disposeIndirectStubsManagerBuilder
  liftIO $ f (StubsMgr stubsMgr)

withJITCompileCallbackManager ::
  String {- ^ triple -} ->
  Maybe (IO ()) ->
  (JITCompileCallbackManager -> IO a) ->
  IO a
withJITCompileCallbackManager triple errorHandler f = flip runAnyContT return $ do
  triple' <- encodeM triple
  errorHandler' <- encodeM errorHandler
  callbackMgr <- anyContToM $ bracket
    (FFI.createLocalCompileCallbackManager triple' errorHandler')
    FFI.disposeCallbackManager
  liftIO $ f (CallbackMgr callbackMgr)

withCompileOnDemandLayer ::
  IRCompileLayer ->
  PartitioningFn ->
  JITCompileCallbackManager ->
  IndirectStubsManagerBuilder ->
  Bool ->
  (CompileOnDemandLayer -> IO a) ->
  IO a
withCompileOnDemandLayer
 baseLayer@(IRCompileLayer base _ _)
 partition
 (CallbackMgr callbackMgr)
 (StubsMgr stubsMgr)
 cloneStubsIntoPartitions
 f
 = flip runAnyContT return $ do
 cleanup <- anyContToM $ bracket (newIORef []) (sequence <=< readIORef)
 partitionAct <- encodeM partition
 partition' <- liftIO $ partitionAct cleanup
 cloneStubsIntoPartitions' <- encodeM cloneStubsIntoPartitions
 cl <- anyContToM $ bracket
         (FFI.createCompileOnDemandLayer
            base
            partition'
            callbackMgr
            stubsMgr
            cloneStubsIntoPartitions')
         FFI.disposeCompileOnDemandLayer
 liftIO $ f (CompileOnDemandLayer cl baseLayer cleanup)

mangleSymbol :: CompileOnDemandLayer -> String -> IO MangledSymbol
mangleSymbol (CompileOnDemandLayer _ bl _) symbol =
  IRCompileLayer.mangleSymbol bl symbol

findSymbol :: CompileOnDemandLayer -> MangledSymbol -> Bool -> IO JITSymbol
findSymbol (CompileOnDemandLayer cl _ _) symbol exportedSymbolsOnly = flip runAnyContT return $ do
  symbol' <- encodeM symbol
  exportedSymbolsOnly' <- encodeM exportedSymbolsOnly
  symbol <- anyContToM $ bracket
    (FFI.findSymbol cl symbol' exportedSymbolsOnly') FFI.disposeSymbol
  decodeM symbol

addModuleSet :: CompileOnDemandLayer -> [Module] -> SymbolResolver -> IO ModuleSet
addModuleSet
  (CompileOnDemandLayer cl (IRCompileLayer _ dl _) cleanups)
  modules
  resolver
  = flip runAnyContT return $ do
  resolverAct <- encodeM resolver
  resolver' <- liftIO $ resolverAct cleanups
  modules' <- liftIO $ mapM readModule modules
  (moduleCount, modules'') <-
    anyContToM $ \f -> withArrayLen modules' $ \n hs -> f (fromIntegral n, hs)
  moduleSet <- liftIO $ FFI.addModuleSet cl dl modules'' moduleCount resolver'
  pure (ModuleSet moduleSet)

removeModuleSet :: CompileOnDemandLayer -> ModuleSet -> IO ()
removeModuleSet (CompileOnDemandLayer cl _ _) (ModuleSet handle) =
  FFI.removeModuleSet cl handle

withModuleSet :: CompileOnDemandLayer -> [Module] -> SymbolResolver -> (ModuleSet -> IO a) -> IO a
withModuleSet compileLayer modules resolver =
  bracket
    (addModuleSet compileLayer modules resolver)
    (removeModuleSet compileLayer)
