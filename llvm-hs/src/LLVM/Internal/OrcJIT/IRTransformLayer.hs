module LLVM.Internal.OrcJIT.IRTransformLayer where

import LLVM.Prelude

import Control.Exception
import Control.Monad.AnyCont
import Control.Monad.IO.Class
import Data.IORef
import Foreign.Ptr

import qualified LLVM.Internal.FFI.DataLayout as FFI
import qualified LLVM.Internal.FFI.Module as FFI
import qualified LLVM.Internal.FFI.OrcJIT.IRTransformLayer as FFI
import qualified LLVM.Internal.FFI.PtrHierarchy as FFI
import LLVM.Internal.OrcJIT
import LLVM.Internal.OrcJIT.CompileLayer
import LLVM.Internal.Target

-- | 'IRTransformLayer' allows transforming modules before handing off
-- compilation to the underlying 'CompileLayer'.
data IRTransformLayer baseLayer =
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

-- | Create a new 'IRTransformLayer'.
--
-- When the layer is no longer needed, it should be disposed using 'disposeCompileLayer'.
newIRTransformLayer
  :: CompileLayer l
  => l
  -> TargetMachine
  -> (Ptr FFI.Module -> IO (Ptr FFI.Module)) {- ^ module transformation -}
  -> IO (IRTransformLayer l)
newIRTransformLayer compileLayer tm moduleTransform =
  flip runAnyContT return $ do
    cleanups <- liftIO (newIORef [])
    dl <- createRegisteredDataLayout tm cleanups
    let encodedModuleTransform =
          allocFunPtr cleanups (FFI.wrapModuleTransform moduleTransform)
    moduleTransform' <-
      anyContToM $ bracketOnError encodedModuleTransform freeHaskellFunPtr
    cl <-
      liftIO
        (FFI.createIRTransformLayer
           (getCompileLayer compileLayer)
           moduleTransform')
    return (IRTransformLayer cl dl cleanups)

-- | 'bracket'-style wrapper around 'newIRTransformLayer' and 'disposeCompileLayer'.
withIRTransformLayer ::
     CompileLayer l
  => l
  -> TargetMachine
  -> (Ptr FFI.Module -> IO (Ptr FFI.Module)) {- ^ module transformation -}
  -> (IRTransformLayer l -> IO a)
  -> IO a
withIRTransformLayer compileLayer tm moduleTransform =
  bracket
    (newIRTransformLayer compileLayer tm moduleTransform)
    disposeCompileLayer
