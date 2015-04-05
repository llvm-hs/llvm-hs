{-# LANGUAGE
  MultiParamTypeClasses,
  UndecidableInstances
  #-}
module LLVM.General.Internal.MemoryBuffer where

import LLVM.General.Prelude

import Control.Exception
import Control.Monad.Exceptable
import Control.Monad.AnyCont
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Foreign.Ptr

import LLVM.General.Internal.Coding
import LLVM.General.Internal.String
import LLVM.General.Internal.Inject
import qualified LLVM.General.Internal.FFI.LLVMCTypes as FFI
import qualified LLVM.General.Internal.FFI.MemoryBuffer as FFI

data Specification 
  = Bytes { name :: String,  content :: BS.ByteString }
  | File { pathName :: String }

instance (Inject String e, MonadError e m, Monad m, MonadIO m, MonadAnyCont IO m) => EncodeM m Specification (FFI.OwnerTransfered (Ptr FFI.MemoryBuffer)) where
  encodeM spec = liftM FFI.OwnerTransfered $ do
    case spec of
      Bytes name content -> do
        (s,l) <- anyContToM $ BS.unsafeUseAsCStringLen (BS.snoc content 0)
        name <- encodeM name
        nullTerminate <- encodeM True
        liftIO $ FFI.createMemoryBufferWithMemoryRange s (fromIntegral (l-1)) name nullTerminate
      File pathName -> do
        pathName <- encodeM pathName
        mbPtr <- alloca
        msgPtr <- alloca
        result <- decodeM =<< (liftIO $ FFI.createMemoryBufferWithContentsOfFile pathName mbPtr msgPtr)
        when result $ do
          msg <- decodeM msgPtr
          throwError (inject (msg :: String))
        peek mbPtr          

instance (Inject String e, MonadError e m, Monad m, MonadIO m, MonadAnyCont IO m) => EncodeM m Specification (Ptr FFI.MemoryBuffer) where
  encodeM spec = do
    FFI.OwnerTransfered mb <- encodeM spec
    anyContToM $ bracket (return mb) FFI.disposeMemoryBuffer

instance MonadIO d => DecodeM d BS.ByteString (Ptr FFI.MemoryBuffer) where
  decodeM p = do
    s <- liftIO $ FFI.getBufferStart p
    l <- liftIO $ FFI.getBufferSize p
    liftIO $ BS.packCStringLen (s, fromIntegral l)

instance MonadIO d => DecodeM d String (Ptr FFI.MemoryBuffer) where
  decodeM = decodeM . UTF8ByteString <=< decodeM
