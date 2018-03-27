{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
module LLVM.Internal.OrcJIT.CompileOnDemandLayer where

import LLVM.Prelude

import Control.Exception
import Control.Monad.AnyCont
import Control.Monad.IO.Class
import Data.IORef
import Foreign.Ptr

import LLVM.Internal.Coding
import LLVM.Internal.OrcJIT hiding (ObjectLinkingLayer(..))
import LLVM.Internal.OrcJIT.CompileLayer
import LLVM.Internal.Target
import qualified LLVM.Internal.FFI.DataLayout as FFI
import qualified LLVM.Internal.FFI.OrcJIT as FFI
import qualified LLVM.Internal.FFI.OrcJIT.CompileOnDemandLayer as FFI
import qualified LLVM.Internal.FFI.PtrHierarchy as FFI

type PartitioningFn = Ptr FFI.Function -> IO [Ptr FFI.Function]

-- | This is used by 'CompileOnDemandLayer' to create callback that
-- compile functions when they are called.
data JITCompileCallbackManager =
  CallbackMgr !(Ptr FFI.JITCompileCallbackManager)
              !(IO ())

-- | This is used by 'CompileOnDemandLayer' to manage the stubs
-- created for function definitions that have not yet been compiled.
newtype IndirectStubsManagerBuilder =
  StubsMgr (Ptr FFI.IndirectStubsManagerBuilder)

-- | Adding a module to a 'CompileOnDemandLayer' creates stubs for its
-- functions definitions. When one of those stubs is called, the
-- corresponding function body is extracted and compiled.
data CompileOnDemandLayer baseLayer =
  CompileOnDemandLayer {
    compileLayer :: !(Ptr FFI.CompileOnDemandLayer),
    dataLayout :: !(Ptr FFI.DataLayout),
    cleanupActions :: !(IORef [IO ()])
  }
  deriving Eq

instance CompileLayer (CompileOnDemandLayer l) where
  getCompileLayer = FFI.upCast . compileLayer
  getDataLayout = dataLayout
  getCleanups = cleanupActions

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
         EncodeM m (Maybe (IO ())) (FFI.TargetAddress, IO ()) where
  encodeM Nothing = return (FFI.TargetAddress 0, return ())
  encodeM (Just f) = do
    f' <- anyContToM $ bracketOnError (FFI.wrapErrorHandler f) freeHaskellFunPtr
    return
      ( (FFI.TargetAddress . fromIntegral . ptrToWordPtr . castFunPtrToPtr) f'
      , freeHaskellFunPtr f')

-- | Create a new 'IndirectStubsManagerBuilder'.
--
-- When the stubs manager is no longer needed, it should be freed
-- using 'disposeIndirectStubsManagerBuilder'.
newIndirectStubsManagerBuilder ::
  ShortByteString {- ^ target triple -} ->
  IO IndirectStubsManagerBuilder
newIndirectStubsManagerBuilder triple =
  flip runAnyContT return $ do
    triple' <- encodeM triple
    stubsMgr <- liftIO (FFI.createLocalIndirectStubsManagerBuilder triple')
    return (StubsMgr stubsMgr)

-- | Dispose of an 'IndirectStubsManagerBuilder'.
disposeIndirectStubsManagerBuilder :: IndirectStubsManagerBuilder -> IO ()
disposeIndirectStubsManagerBuilder (StubsMgr stubsMgr) =
  FFI.disposeIndirectStubsManagerBuilder stubsMgr

-- | 'bracket'-style wrapper around 'newIndirectStubsManagerBuilder'
-- and 'disposeIndirectStubsManagerBuilder'.
withIndirectStubsManagerBuilder ::
  ShortByteString {- ^ target triple -} ->
  (IndirectStubsManagerBuilder -> IO a) ->
  IO a
withIndirectStubsManagerBuilder triple =
  bracket
    (newIndirectStubsManagerBuilder triple)
    disposeIndirectStubsManagerBuilder

-- | Create a new 'JITCompileCallbackManager'.
--
-- When the callback manager is no longer needed, it should be freed
-- using 'disposeJITCompileCallbackManager'.
newJITCompileCallbackManager ::
  ShortByteString {- ^ target triple -} ->
  Maybe (IO ()) {- ^ called on compilation errors -} ->
  IO JITCompileCallbackManager
newJITCompileCallbackManager triple errorHandler = flip runAnyContT return $ do
  triple' <- encodeM triple
  (errorHandler', cleanup) <- encodeM errorHandler
  callbackMgr <- liftIO (FFI.createLocalCompileCallbackManager triple' errorHandler')
  return (CallbackMgr callbackMgr cleanup)

-- | Dispose of a 'JITCompileCallbackManager'.
disposeJITCompileCallbackManager :: JITCompileCallbackManager -> IO ()
disposeJITCompileCallbackManager (CallbackMgr mgr cleanup) =
  FFI.disposeCallbackManager mgr >> cleanup

-- | Execute a computation using a new 'JITCompileCallbackManager'.
withJITCompileCallbackManager ::
  ShortByteString {- ^ target triple -} ->
  Maybe (IO ()) {- ^ called on compilation errors -} ->
  (JITCompileCallbackManager -> IO a) ->
  IO a
withJITCompileCallbackManager triple errorHandler =
  bracket
    (newJITCompileCallbackManager triple errorHandler)
    disposeJITCompileCallbackManager

-- | Create a new 'CompileOnDemandLayer'. The partitioning function
-- specifies which functions should be compiled when a function is
-- called.
--
-- When the layer is no longer needed, it should be disposed using 'disposeCompileLayer'.
newCompileOnDemandLayer :: CompileLayer l =>
  l ->
  TargetMachine ->
  (Ptr FFI.Function -> IO [Ptr FFI.Function]) {- ^ partitioning function -} ->
  JITCompileCallbackManager ->
  IndirectStubsManagerBuilder ->
  Bool {- ^ clone stubs into partitions -} ->
  IO (CompileOnDemandLayer l)
newCompileOnDemandLayer baseLayer tm partition (CallbackMgr callbackMgr _) (StubsMgr stubsMgr) cloneStubs =
  flip runAnyContT return $ do
    cleanups <- liftIO (newIORef [])
    dl <- createRegisteredDataLayout tm cleanups
    partitionAct <- encodeM partition
    partition' <- liftIO $ partitionAct cleanups
    cloneStubs' <- encodeM cloneStubs
    cl <-
      liftIO
        (FFI.createCompileOnDemandLayer
           (getCompileLayer baseLayer)
           partition'
           callbackMgr
           stubsMgr
           cloneStubs')
    return (CompileOnDemandLayer cl dl cleanups)

-- | 'bracket'-style wrapper around 'newCompileOnDemandLayer' and 'disposeCompileLayer'.
withCompileOnDemandLayer ::
  CompileLayer l =>
  l ->
  TargetMachine ->
  (Ptr FFI.Function -> IO [Ptr FFI.Function]) {- ^ partitioning function -} ->
  JITCompileCallbackManager ->
  IndirectStubsManagerBuilder ->
  Bool {- ^ clone stubs into partitions -} ->
  (CompileOnDemandLayer l -> IO a) ->
  IO a
withCompileOnDemandLayer l tm partition callbackMgr stubsMgr cloneStubs =
  bracket
    (newCompileOnDemandLayer l tm partition callbackMgr stubsMgr cloneStubs)
    disposeCompileLayer
