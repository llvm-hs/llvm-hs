module LLVM.Internal.OrcJIT.IRCompileLayer where

import LLVM.Prelude

import Control.Exception
import Control.Monad.AnyCont
import Control.Monad.IO.Class
import Data.IORef
import Foreign.Ptr

import qualified LLVM.Internal.FFI.DataLayout as FFI
import qualified LLVM.Internal.FFI.OrcJIT.CompileLayer as FFI
import qualified LLVM.Internal.FFI.OrcJIT.IRCompileLayer as FFI
import qualified LLVM.Internal.FFI.PtrHierarchy as FFI
import qualified LLVM.Internal.FFI.Target as FFI
import LLVM.Internal.OrcJIT
import LLVM.Internal.OrcJIT.CompileLayer
import LLVM.Internal.Target

data IRCompileLayer linkingLayer =
  IRCompileLayer {
    compileLayer :: !(Ptr FFI.IRCompileLayer),
    dataLayout :: !(Ptr FFI.DataLayout),
    cleanupActions :: !(IORef [IO ()])
  }
  deriving Eq

instance CompileLayer (IRCompileLayer l) where
  getCompileLayer = FFI.upCast . compileLayer
  getDataLayout = dataLayout
  getCleanups = cleanupActions

withIRCompileLayer :: LinkingLayer l => l -> TargetMachine -> (IRCompileLayer l -> IO a) -> IO a
withIRCompileLayer linkingLayer (TargetMachine tm) f = flip runAnyContT return $ do
  dl <- anyContToM $ bracket (FFI.createTargetDataLayout tm) FFI.disposeDataLayout
  cl <- anyContToM $ bracket (FFI.createIRCompileLayer (getLinkingLayer linkingLayer) tm) (FFI.disposeCompileLayer . FFI.upCast)
  cleanup <- anyContToM $ bracket (newIORef []) (sequence <=< readIORef)
  liftIO $ f (IRCompileLayer cl dl cleanup)
