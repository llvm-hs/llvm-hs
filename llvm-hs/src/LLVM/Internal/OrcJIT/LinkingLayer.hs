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
import qualified LLVM.Internal.FFI.ShortByteString as SBS
import qualified LLVM.Internal.FFI.PtrHierarchy as FFI
import qualified LLVM.Internal.FFI.OrcJIT as FFI
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

-- | @'findSymbol' layer symbol exportedSymbolsOnly@ searches for
-- @symbol@ in all modules added to @layer@. If @exportedSymbolsOnly@
-- is 'True' only exported symbols are searched.
findSymbol :: LinkingLayer l => l -> ShortByteString -> Bool -> IO JITSymbol
findSymbol linkingLayer symbol exportedSymbolsOnly =
  SBS.useAsCString symbol $ \symbol' ->
    flip runAnyContT return $ do
      exportedSymbolsOnly' <- encodeM exportedSymbolsOnly
      symbol <- anyContToM $ bracket
        (FFI.findSymbol (getLinkingLayer linkingLayer) symbol' exportedSymbolsOnly') FFI.disposeSymbol
      decodeM symbol

-- | @'findSymbolIn' layer handle symbol exportedSymbolsOnly@ searches for
-- @symbol@ in the context of the module represented by @handle@. If
-- @exportedSymbolsOnly@ is 'True' only exported symbols are searched.
findSymbolIn :: LinkingLayer l => l -> FFI.ObjectHandle -> ShortByteString -> Bool -> IO JITSymbol
findSymbolIn linkingLayer handle symbol exportedSymbolsOnly =
  SBS.useAsCString symbol $ \symbol' ->
    flip runAnyContT return $ do
      exportedSymbolsOnly' <- encodeM exportedSymbolsOnly
      symbol <- anyContToM $ bracket
        (FFI.findSymbolIn (getLinkingLayer linkingLayer) handle symbol' exportedSymbolsOnly') FFI.disposeSymbol
      decodeM symbol
