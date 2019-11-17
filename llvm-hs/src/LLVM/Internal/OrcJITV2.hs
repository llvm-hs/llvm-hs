module LLVM.Internal.OrcJITV2
  ( ExecutionSession
  , withExecutionSession
  , esLookup
  , ThreadSafeContext
  , withThreadSafeContext
  , ObjectLayer
  , withRTDyldObjectLinkingLayer
  , IRLayer
  , withIRCompileLayer
  , irLayerAdd
  ) where

import LLVM.Prelude

import Control.Exception
import Foreign.C
import Foreign.Ptr

import LLVM.Internal.Module (Module, readModule, deleteModule)
import LLVM.Internal.OrcJIT (ExecutionSession(..), withExecutionSession)
import LLVM.Internal.Target (TargetMachine(..))

import qualified LLVM.Internal.FFI.DataLayout as FFI
import qualified LLVM.Internal.FFI.OrcJITV2 as FFI
import qualified LLVM.Internal.FFI.Target as FFI

newtype ThreadSafeContext = ThreadSafeContext (Ptr FFI.ThreadSafeContext)
data IRLayer = IRLayer
  { _getIRLayer :: Ptr FFI.IRLayer
  , _getDataLayout :: Ptr FFI.DataLayout
  }
newtype ObjectLayer = ObjectLayer (Ptr FFI.ObjectLayer)

esLookup :: ExecutionSession -> String -> IO Word64
esLookup (ExecutionSession es) s = withCString s $ \cStr ->
  FFI.esLookup es cStr

createThreadSafeContext :: IO ThreadSafeContext
createThreadSafeContext = ThreadSafeContext <$> FFI.createThreadSafeContext

disposeThreadSafeContext :: ThreadSafeContext -> IO ()
disposeThreadSafeContext (ThreadSafeContext ctx) = FFI.disposeThreadSafeContext ctx

withThreadSafeContext :: (ThreadSafeContext -> IO a) -> IO a
withThreadSafeContext = bracket createThreadSafeContext disposeThreadSafeContext

createRTDyldObjectLinkingLayer :: ExecutionSession -> IO ObjectLayer
createRTDyldObjectLinkingLayer (ExecutionSession es) =
  ObjectLayer <$> FFI.createRTDyldObjectLinkingLayer es

disposeObjectLayer :: ObjectLayer -> IO ()
disposeObjectLayer (ObjectLayer ol) = FFI.disposeObjectLayer ol

withRTDyldObjectLinkingLayer :: ExecutionSession -> (ObjectLayer -> IO a) -> IO a
withRTDyldObjectLinkingLayer es =
  bracket
    (createRTDyldObjectLinkingLayer es)
    disposeObjectLayer

createIRCompileLayer :: ExecutionSession -> ObjectLayer -> TargetMachine -> IO IRLayer
createIRCompileLayer (ExecutionSession es) (ObjectLayer ol) (TargetMachine tm) = do
  dl <- FFI.createTargetDataLayout tm
  il <- FFI.createIRCompileLayer es ol tm
  pure $ IRLayer il dl

disposeIRLayer :: IRLayer -> IO ()
disposeIRLayer (IRLayer il _) = FFI.disposeIRLayer il

withIRCompileLayer :: ExecutionSession -> ObjectLayer -> TargetMachine -> (IRLayer -> IO a) -> IO a
withIRCompileLayer es ol tm =
  bracket
    (createIRCompileLayer es ol tm)
    disposeIRLayer

irLayerAdd :: ThreadSafeContext -> ExecutionSession -> IRLayer -> Module -> IO ()
irLayerAdd (ThreadSafeContext ctx) (ExecutionSession es) (IRLayer il dl) m = do
  mPtr <- readModule m
  deleteModule m
  FFI.irLayerAdd ctx es dl il mPtr
