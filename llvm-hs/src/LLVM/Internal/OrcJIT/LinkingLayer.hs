module LLVM.Internal.OrcJIT.LinkingLayer where

import LLVM.Prelude

import Control.Exception
import Control.Monad.AnyCont
import Control.Monad.IO.Class
import Data.IORef
import Foreign.Ptr

import LLVM.Internal.OrcJIT
import LLVM.Internal.Coding
import LLVM.Internal.ObjectFile
import qualified LLVM.Internal.FFI.PtrHierarchy as FFI
import qualified LLVM.Internal.FFI.OrcJIT.LinkingLayer as FFI

-- | After a 'CompileLayer' has compiled the modules to object code,
-- it passes the resulting object files to a 'LinkingLayer'.
class LinkingLayer l where
  getLinkingLayer :: l -> Ptr FFI.LinkingLayer
  getCleanups :: l -> IORef [IO ()]

-- | Dispose of a 'LinkingLayer'.
disposeLinkingLayer :: LinkingLayer l => l -> IO ()
disposeLinkingLayer l = do
  FFI.disposeLinkingLayer (getLinkingLayer l)
  sequence_ =<< readIORef (getCleanups l)

-- | Add an object file to the 'LinkingLayer'. The 'SymbolResolver' is used
-- to resolve external symbols in the module.
addObjectFile :: LinkingLayer l => l -> ObjectFile -> SymbolResolver
              -> IO FFI.ObjectHandle
addObjectFile linkingLayer (ObjectFile obj) resolver = flip runAnyContT return $ do
  resolverAct <- encodeM resolver
  resolver'   <- liftIO $ resolverAct (getCleanups linkingLayer)
  errMsg <- alloca
  liftIO $
    FFI.addObjectFile
      (getLinkingLayer linkingLayer)
      obj
      resolver'
      errMsg

-- | Bare bones implementation of a 'LinkingLayer'.
data ObjectLinkingLayer = ObjectLinkingLayer {
   linkingLayer :: !(Ptr FFI.ObjectLinkingLayer),
   cleanupActions :: !(IORef [IO ()])
  }

instance LinkingLayer ObjectLinkingLayer where
  getLinkingLayer (ObjectLinkingLayer ptr _) = FFI.upCast ptr
  getCleanups = cleanupActions

-- | Create a new 'ObjectLinkingLayer'. This should be disposed using
-- 'disposeLinkingLayer' when it is no longer needed.
newObjectLinkingLayer :: IO ObjectLinkingLayer
newObjectLinkingLayer = do
  linkingLayer <- FFI.createObjectLinkingLayer
  cleanups <- liftIO (newIORef [])
  return $ ObjectLinkingLayer linkingLayer cleanups

-- | 'bracket'-style wrapper around 'newObjectLinkingLayer' and 'disposeLinkingLayer'.
withObjectLinkingLayer :: (ObjectLinkingLayer -> IO a) -> IO a
withObjectLinkingLayer = bracket newObjectLinkingLayer disposeLinkingLayer
