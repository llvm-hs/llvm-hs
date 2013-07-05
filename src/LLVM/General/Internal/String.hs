{-# LANGUAGE
  MultiParamTypeClasses,
  UndecidableInstances
  #-}
module LLVM.General.Internal.String where

import Control.Arrow
import Control.Monad
import Foreign.C (CString, CChar)
import Foreign.Ptr
import Control.Monad.AnyCont
import Control.Monad.IO.Class
import Foreign.Storable (Storable)
import Foreign.Marshal.Alloc as F.M (alloca)

import LLVM.General.Internal.Coding

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

newtype UTF8ByteString = UTF8ByteString { utf8Bytes :: BS.ByteString }

instance (Monad e) => EncodeM e String UTF8ByteString where
  encodeM = return . UTF8ByteString . T.encodeUtf8 . T.pack

instance (Monad d) => DecodeM d String UTF8ByteString where
  decodeM = return . T.unpack . T.decodeUtf8 . utf8Bytes


instance (MonadAnyCont IO e) => EncodeM e String CString where
  encodeM s = anyContToM (BS.useAsCString . utf8Bytes =<< encodeM s)

instance (Integral i, MonadAnyCont IO e) => EncodeM e String (Ptr CChar, i) where
  encodeM s = anyContToM ((. (. second fromIntegral)) $ BS.useAsCStringLen . utf8Bytes =<< encodeM s)

instance (MonadIO d) => DecodeM d String CString where
  decodeM = decodeM . UTF8ByteString <=< liftIO . BS.packCString

instance (Integral i, MonadIO d) => DecodeM d String (Ptr CChar, i) where
  decodeM = decodeM . UTF8ByteString <=< liftIO . BS.packCStringLen . second fromIntegral

instance (Integral i, Storable i, MonadIO d) => DecodeM d String (Ptr i -> IO (Ptr CChar)) where
  decodeM f = decodeM =<< (liftIO $ F.M.alloca $ \p -> (,) `liftM` f p `ap` peek p)

