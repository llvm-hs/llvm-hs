{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
module LLVM.Internal.OrcJIT.CompileOnDemandLayer where

import LLVM.Prelude

import Control.Exception
import Control.Monad.AnyCont
import Control.Monad.IO.Class
import Data.IORef
import Data.Set (Set)
import Foreign.Ptr

import LLVM.Internal.Coding
import LLVM.Internal.OrcJIT
import LLVM.Internal.OrcJIT.CompileLayer
import LLVM.Internal.Target
import qualified LLVM.Internal.FFI.DataLayout as FFI
import qualified LLVM.Internal.FFI.Target as FFI
import qualified LLVM.Internal.FFI.OrcJIT as FFI
import qualified LLVM.Internal.FFI.OrcJIT.CompileLayer as FFI
import qualified LLVM.Internal.FFI.OrcJIT.CompileOnDemandLayer as FFI
import qualified LLVM.Internal.FFI.PtrHierarchy as FFI

type PartitioningFn = Ptr FFI.Function -> IO [Ptr FFI.Function]

-- | This is used by 'CompileOnDemandLayer' to create callback that
-- compile functions when they are called.
newtype JITCompileCallbackManager =
  CallbackMgr (Ptr FFI.JITCompileCallbackManager)

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
  EncodeM m (Maybe (IO ())) FFI.TargetAddress where
  encodeM Nothing = return $ FFI.TargetAddress 0
  encodeM (Just f) = do
    f' <- anyContToM $ bracket (FFI.wrapErrorHandler f) freeHaskellFunPtr
    return . FFI.TargetAddress . fromIntegral . ptrToWordPtr . castFunPtrToPtr $ f'

-- | Execute a computation using a new
-- 'IndirectStubsManagerBuilder'.
withIndirectStubsManagerBuilder ::
  ShortByteString {- ^ target triple -} ->
  (IndirectStubsManagerBuilder -> IO a) ->
  IO a
withIndirectStubsManagerBuilder triple f = flip runAnyContT return $ do
  triple' <- encodeM triple
  stubsMgr <- anyContToM $ bracket
    (FFI.createLocalIndirectStubsManagerBuilder triple')
    FFI.disposeIndirectStubsManagerBuilder
  liftIO $ f (StubsMgr stubsMgr)

-- | Execute a computation using a new 'JITCompileCallbackManager'.
withJITCompileCallbackManager ::
  ShortByteString {- ^ target triple -} ->
  Maybe (IO ()) {- ^ called on compilation errors -} ->
  (JITCompileCallbackManager -> IO a) ->
  IO a
withJITCompileCallbackManager triple errorHandler f = flip runAnyContT return $ do
  triple' <- encodeM triple
  errorHandler' <- encodeM errorHandler
  callbackMgr <- anyContToM $ bracket
    (FFI.createLocalCompileCallbackManager triple' errorHandler')
    FFI.disposeCallbackManager
  liftIO $ f (CallbackMgr callbackMgr)

-- | Execute a computation using a new 'CompileOnDemandLayer'. The
-- partitioning function specifies which functions should be compiled
-- when a function is called.
withCompileOnDemandLayer :: CompileLayer l =>
  l ->
  TargetMachine ->
  (Ptr FFI.Function -> IO [Ptr FFI.Function]) {- ^ partitioning function -} ->
  JITCompileCallbackManager ->
  IndirectStubsManagerBuilder ->
  Bool ->
  (CompileOnDemandLayer l -> IO a) ->
  IO a
withCompileOnDemandLayer
 baseLayer
 (TargetMachine tm)
 partition
 (CallbackMgr callbackMgr)
 (StubsMgr stubsMgr)
 cloneStubsIntoPartitions
 f
 = flip runAnyContT return $ do
 dl <- anyContToM $ bracket (FFI.createTargetDataLayout tm) FFI.disposeDataLayout
 cleanup <- anyContToM $ bracket (newIORef []) (sequence <=< readIORef)
 partitionAct <- encodeM partition
 partition' <- liftIO $ partitionAct cleanup
 cloneStubsIntoPartitions' <- encodeM cloneStubsIntoPartitions
 cl <- anyContToM $ bracket
         (FFI.createCompileOnDemandLayer
            (getCompileLayer baseLayer)
            partition'
            callbackMgr
            stubsMgr
            cloneStubsIntoPartitions')
         (FFI.disposeCompileLayer . FFI.upCast)
 liftIO $ f (CompileOnDemandLayer cl dl cleanup)
