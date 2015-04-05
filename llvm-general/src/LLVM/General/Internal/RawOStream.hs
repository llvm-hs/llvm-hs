module LLVM.General.Internal.RawOStream where

import LLVM.General.Prelude

import Control.Monad.Exceptable
import Control.Monad.AnyCont

import Data.IORef
import Foreign.C
import Foreign.Ptr

import qualified  LLVM.General.Internal.FFI.RawOStream as FFI

import LLVM.General.Internal.Coding
import LLVM.General.Internal.Inject
import LLVM.General.Internal.String ()

withFileRawOStream ::
  (Inject String e, MonadError e m, MonadAnyCont IO m, MonadIO m)
  => String
  -> Bool
  -> Bool
  -> (Ptr FFI.RawOStream -> ExceptT String IO ())
  -> m ()
withFileRawOStream path excl text c = do
  path <- encodeM path
  excl <- encodeM excl
  text <- encodeM text
  msgPtr <- alloca
  errorRef <- liftIO $ newIORef undefined
  succeeded <- decodeM =<< (liftIO $ FFI.withFileRawOStream path excl text msgPtr $ \os -> do
                              r <- runExceptableT (ExceptableT  $ c os)
                              writeIORef errorRef r)
  unless succeeded $ do
    s <- decodeM msgPtr
    throwError $ inject (s :: String)
  e <- liftIO $ readIORef errorRef
  either (throwError . inject) return e

withBufferRawOStream ::
  (Inject String e, MonadError e m, MonadIO m, DecodeM IO a (Ptr CChar, CSize))
  => (Ptr FFI.RawOStream -> ExceptT String IO ())
  -> m a
withBufferRawOStream c = do
  resultRef <- liftIO $ newIORef Nothing
  errorRef <- liftIO $ newIORef undefined
  let saveBuffer :: Ptr CChar -> CSize -> IO ()
      saveBuffer start size = do
        r <- decodeM (start, size)
        writeIORef resultRef (Just r)
      saveError os = do
        r <- runExceptableT (ExceptableT $ c os)
        writeIORef errorRef r
  liftIO $ FFI.withBufferRawOStream saveBuffer saveError
  e <- liftIO $ readIORef errorRef
  case e of
    Left e -> throwError $ inject e
    _ -> do
      Just r <- liftIO $ readIORef resultRef
      return r
