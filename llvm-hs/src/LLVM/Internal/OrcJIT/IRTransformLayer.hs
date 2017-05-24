module LLVM.Internal.OrcJIT.IRTransformLayer where

import LLVM.Prelude

import Control.Exception
import Control.Monad.AnyCont
import Control.Monad.IO.Class
import Data.IORef
import Foreign.Ptr

import qualified LLVM.Internal.FFI.DataLayout as FFI
import qualified LLVM.Internal.FFI.Module as FFI
import qualified LLVM.Internal.FFI.OrcJIT.CompileLayer as FFI
import qualified LLVM.Internal.FFI.OrcJIT.IRTransformLayer as FFI
import qualified LLVM.Internal.FFI.PtrHierarchy as FFI
import qualified LLVM.Internal.FFI.Target as FFI
import LLVM.Internal.OrcJIT
import LLVM.Internal.OrcJIT.CompileLayer
import LLVM.Internal.Target

data IRTransformLayer objectLayer =
  IRTransformLayer {
    compileLayer :: !(Ptr FFI.IRTransformLayer),
    dataLayout :: !(Ptr FFI.DataLayout),
    cleanupActions :: !(IORef [IO ()])
  }
  deriving Eq

instance CompileLayer (IRTransformLayer l) where
  getCompileLayer = FFI.upCast . compileLayer
  getDataLayout = dataLayout
  getCleanups = cleanupActions

withIRTransformLayer
  :: CompileLayer l
  => l
  -> TargetMachine
  -> (Ptr FFI.Module -> IO (Ptr FFI.Module))
  -> (IRTransformLayer l -> IO a)
  -> IO a
withIRTransformLayer compileLayer (TargetMachine tm) moduleTransform f =
  flip runAnyContT return $ do
    dl <-
      anyContToM $ bracket (FFI.createTargetDataLayout tm) FFI.disposeDataLayout
    moduleTransform' <-
      anyContToM $
      bracket (FFI.wrapModuleTransform moduleTransform) freeHaskellFunPtr
    cl <-
      anyContToM $
      bracket
        (FFI.createIRTransformLayer
           (getCompileLayer compileLayer)
           moduleTransform')
        (FFI.disposeCompileLayer . FFI.upCast)
    cleanup <- anyContToM $ bracket (newIORef []) (sequence <=< readIORef)
    liftIO $ f (IRTransformLayer cl dl cleanup)
