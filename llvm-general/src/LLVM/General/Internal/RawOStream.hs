module LLVM.General.Internal.RawOStream where

import Control.Monad
import Control.Monad.Error
import Control.Monad.AnyCont

import Data.IORef
import Foreign.C
import Foreign.Ptr

import qualified  LLVM.General.Internal.FFI.RawOStream as FFI

import LLVM.General.Internal.Coding
import LLVM.General.Internal.String ()

withFileRawOStream :: 
  (MonadAnyCont IO m, MonadIO m) 
  => String
  -> Bool
  -> Bool
  -> (Ptr FFI.RawOStream -> ErrorT String IO ())
  -> m ()
withFileRawOStream path excl binary c = do
  path <- encodeM path
  excl <- encodeM excl
  binary <- encodeM binary
  outputRef <- liftIO $ newIORef Nothing
  msgPtr <- alloca
  errorRef <- liftIO $ newIORef undefined
  failed <- decodeM =<< (liftIO $ FFI.withFileRawOStream path excl binary msgPtr $ \os -> do
                           r <- runErrorT (c os)
                           writeIORef errorRef r)
  when failed $ fail =<< decodeM msgPtr
  e <- liftIO $ readIORef errorRef
  case e of
    Left e -> fail e
    _ -> do
      Just r <- liftIO $ readIORef outputRef
      return r

withBufferRawOStream :: 
  (MonadIO m, DecodeM IO a (Ptr CChar, CSize))
  => (Ptr FFI.RawOStream -> ErrorT String IO ())
  -> m a
withBufferRawOStream c = do
  resultRef <- liftIO $ newIORef Nothing
  errorRef <- liftIO $ newIORef undefined
  let saveBuffer :: Ptr CChar -> CSize -> IO ()
      saveBuffer start size = do
        r <- decodeM (start, size)
        writeIORef resultRef (Just r)
      saveError os = do
        r <- runErrorT (c os)
        writeIORef errorRef r
  liftIO $ FFI.withBufferRawOStream saveBuffer saveError
  e <- liftIO $ readIORef errorRef
  case e of
    Left e -> fail e
    _ -> do
      Just r <- liftIO $ readIORef resultRef
      return r
