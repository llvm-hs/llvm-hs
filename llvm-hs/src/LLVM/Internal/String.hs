{-# LANGUAGE
  MultiParamTypeClasses,
  UndecidableInstances
  #-}
module LLVM.Internal.String where

import LLVM.Prelude

import Control.Arrow
import Control.Monad.AnyCont
import Control.Monad.IO.Class
import Control.Exception (finally)
import Foreign.C (CString, CChar)
import Foreign.Ptr
import Foreign.Storable (Storable)
import Foreign.Marshal.Alloc as F.M (alloca, free)

import LLVM.Internal.FFI.LLVMCTypes

import LLVM.Internal.Coding

import qualified Data.ByteString as BS
import qualified LLVM.Internal.FFI.ShortByteString as SBS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.UTF8 as BSUTF8

newtype UTF8ByteString = UTF8ByteString { utf8Bytes :: BS.ByteString }

instance (Monad e) => EncodeM e String UTF8ByteString where
  encodeM = return . UTF8ByteString . BSUTF8.fromString

instance (Monad d) => DecodeM d String UTF8ByteString where
  decodeM = return . BSUTF8.toString . utf8Bytes

instance (MonadAnyCont IO e) => EncodeM e String CString where
  encodeM s = anyContToM (BS.unsafeUseAsCString . utf8Bytes =<< encodeM (s ++ "\0"))

instance (MonadAnyCont IO e) => EncodeM e ByteString CString where
  encodeM s = anyContToM (BS.useAsCString s)

instance (MonadAnyCont IO e) => EncodeM e ShortByteString CString where
  encodeM s = anyContToM (SBS.useAsCString s)

instance (Integral i, MonadAnyCont IO e) => EncodeM e String (Ptr CChar, i) where
  encodeM s = anyContToM ((. (. second fromIntegral)) $ BS.useAsCStringLen . utf8Bytes =<< encodeM s)

instance (Integral i, MonadAnyCont IO e) => EncodeM e ByteString (Ptr CChar, i) where
  encodeM s =
    anyContToM (\cont -> BS.useAsCStringLen s (\(ptr, len) -> cont (ptr, fromIntegral len)))

instance (Integral i, MonadAnyCont IO e) => EncodeM e ShortByteString (Ptr CChar, i) where
  encodeM s =
    anyContToM (\cont -> SBS.useAsCStringLen s (\(ptr, len) -> cont (ptr, fromIntegral len)))

instance (MonadIO d) => DecodeM d String CString where
  decodeM = decodeM . UTF8ByteString <=< liftIO . BS.packCString

instance (MonadIO d) => DecodeM d ByteString CString where
  decodeM = liftIO . BS.packCString

instance (MonadIO d) => DecodeM d ShortByteString CString where
  decodeM = liftIO . SBS.packCString

instance (MonadIO d) => DecodeM d String (OwnerTransfered CString) where
  decodeM (OwnerTransfered s) = liftIO $ finally (decodeM s) (free s)

instance (MonadIO d) => DecodeM d ByteString (OwnerTransfered CString) where
  decodeM (OwnerTransfered s) = liftIO $ finally (decodeM s) (free s)

instance (MonadIO d) => DecodeM d ShortByteString (OwnerTransfered CString) where
  decodeM (OwnerTransfered s) = liftIO $ finally (decodeM s) (free s)
instance (MonadIO d, DecodeM IO s (OwnerTransfered CString)) =>DecodeM d s (Ptr (OwnerTransfered CString)) where
  decodeM = liftIO . decodeM <=< peek

instance (Integral i, MonadIO d) => DecodeM d String (Ptr CChar, i) where
  decodeM = decodeM . UTF8ByteString <=< liftIO . BS.packCStringLen . second fromIntegral

instance (Integral i, MonadIO d) => DecodeM d BS.ByteString (Ptr CChar, i) where
  decodeM = liftIO . BS.packCStringLen . second fromIntegral

instance (Integral i, MonadIO d) => DecodeM d ShortByteString (Ptr CChar, i) where
  decodeM = liftIO . SBS.packCStringLen . second fromIntegral

instance (Integral i, Storable i, MonadIO d, DecodeM d s (CString, i)) => DecodeM d s (Ptr i -> IO CString) where
  decodeM f = decodeM =<< (liftIO $ F.M.alloca $ \p -> (,) `liftM` f p `ap` peek p)

instance (Monad e, EncodeM e String c) => EncodeM e (Maybe String) (NothingAsEmptyString c) where
  encodeM = liftM NothingAsEmptyString . encodeM . fromMaybe ""
  
