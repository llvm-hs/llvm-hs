{-# LANGUAGE
  MultiParamTypeClasses,
  UndecidableInstances
  #-}
module LLVM.General.Internal.MemoryBuffer where

import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString
import Foreign.Ptr

import LLVM.General.Internal.Coding
import LLVM.General.Internal.String
import qualified LLVM.General.Internal.FFI.MemoryBuffer as FFI

instance MonadIO d => DecodeM d ByteString (Ptr FFI.MemoryBuffer) where
  decodeM p = do
    s <- liftIO $ FFI.getBufferStart p
    l <- liftIO $ FFI.getBufferSize p
    liftIO $ packCStringLen (s, fromIntegral l)

instance MonadIO d => DecodeM d String (Ptr FFI.MemoryBuffer) where
  decodeM = decodeM . UTF8ByteString <=< decodeM
