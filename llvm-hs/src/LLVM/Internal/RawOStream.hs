module LLVM.Internal.RawOStream where

import LLVM.Prelude

import Control.Monad.AnyCont
import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.IORef
import Foreign.C
import Foreign.Ptr

import LLVM.Exception

import qualified LLVM.Internal.FFI.RawOStream as FFI
import qualified LLVM.Internal.FFI.PtrHierarchy as FFI

import LLVM.Internal.Coding
import LLVM.Internal.String ()

-- May throw 'FdStreamException'.
withFileRawOStream ::
  (MonadThrow m, MonadIO m, MonadAnyCont IO m)
  => String
  -> Bool
  -> Bool
  -> (Ptr FFI.RawOStream -> IO ())
  -> m ()
withFileRawOStream path excl text c =
  withFileRawPWriteStream path excl text (c . FFI.upCast)

-- May throw 'FdStreamException'.
withFileRawPWriteStream ::
  (MonadThrow m, MonadIO m, MonadAnyCont IO m)
  => String
  -> Bool
  -> Bool
  -> (Ptr FFI.RawPWriteStream -> IO ())
  -> m ()
withFileRawPWriteStream path excl text c = do
  path <- encodeM path
  excl <- encodeM excl
  text <- encodeM text
  msgPtr <- alloca
  succeeded <- decodeM =<< (liftIO $ FFI.withFileRawPWriteStream path excl text msgPtr c)
  unless succeeded $ do
    s <- decodeM msgPtr
    throwM $ FdStreamException s

withBufferRawOStream ::
  (MonadIO m, DecodeM IO a (Ptr CChar, CSize))
  => (Ptr FFI.RawOStream -> IO ())
  -> m a
withBufferRawOStream c = withBufferRawPWriteStream (c . FFI.upCast)

withBufferRawPWriteStream ::
  (MonadIO m, DecodeM IO a (Ptr CChar, CSize))
  => (Ptr FFI.RawPWriteStream -> IO ())
  -> m a
withBufferRawPWriteStream c = liftIO $ do
  resultRef <- newIORef undefined
  let saveBuffer :: Ptr CChar -> CSize -> IO ()
      saveBuffer start size = do
        r <- decodeM (start, size)
        writeIORef resultRef r
  FFI.withBufferRawPWriteStream saveBuffer c
  readIORef resultRef
