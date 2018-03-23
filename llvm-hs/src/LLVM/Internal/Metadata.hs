{-# LANGUAGE
  MultiParamTypeClasses
  #-}
module LLVM.Internal.Metadata where

import LLVM.Prelude

import Control.Monad.State hiding (mapM, forM)
import Control.Monad.AnyCont

import Foreign.Ptr

import qualified Foreign.Marshal.Array as FMA
import qualified Data.Array as Array

import qualified LLVM.Internal.FFI.LLVMCTypes as FFI
import qualified LLVM.Internal.FFI.Metadata as FFI
import qualified LLVM.Internal.FFI.PtrHierarchy as FFI

import LLVM.Internal.Context
import LLVM.Internal.Coding
import LLVM.Internal.EncodeAST
import LLVM.Internal.DecodeAST
import LLVM.Internal.Value ()

import Foreign.C

instance EncodeM EncodeAST ShortByteString FFI.MDKindID where
  encodeM s = do
    Context c <- gets encodeStateContext
    s <- encodeM s
    liftIO $ FFI.getMDKindIDInContext c s

getMetadataKindNames :: Context -> DecodeAST ()
getMetadataKindNames (Context c) = scopeAnyCont $ do
  let g n = do
        ps <- allocaArray n
        ls <- allocaArray n
        n' <- liftIO $ FFI.getMDKindNames c ps ls n
        if n' > n
         then g n'
         else do
           csls <- return zip
                   `ap` liftIO (FMA.peekArray (fromIntegral n') ps)
                   `ap` liftIO (FMA.peekArray (fromIntegral n') ls)
           mapM decodeM csls
  strs <- g 16
  modify $ \s -> s { metadataKinds = Array.listArray (0, fromIntegral (length strs) - 1) strs }

instance DecodeM DecodeAST ShortByteString FFI.MDKindID where
  decodeM (FFI.MDKindID k) = gets $ (Array.! (fromIntegral k)) . metadataKinds

instance DecodeM DecodeAST ShortByteString (Ptr FFI.MDString) where
  -- LLVM appears to use null pts to indicate empty byte string fields
  -- including literal empty strings
  decodeM = getByteStringFromFFI FFI.getMDStringValue

getByteStringFromFFI :: (Ptr a -> Ptr CUInt -> IO CString) -> Ptr a -> DecodeAST ShortByteString
getByteStringFromFFI _ p | nullPtr == p = return mempty
getByteStringFromFFI f p = do
  np <- alloca
  s <- liftIO $ f p np
  n <- peek np
  decodeM (s, n)
