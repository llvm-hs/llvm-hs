{-# LANGUAGE
  MultiParamTypeClasses
  #-}
module LLVM.General.Internal.Metadata where

import LLVM.General.Prelude

import Control.Monad.State
import Control.Monad.AnyCont

import Foreign.Ptr

import qualified Foreign.Marshal.Array as FMA
import qualified Data.Array as Array

import qualified LLVM.General.Internal.FFI.LLVMCTypes as FFI
import qualified LLVM.General.Internal.FFI.Metadata as FFI
import qualified LLVM.General.Internal.FFI.PtrHierarchy as FFI

import LLVM.General.Internal.Context
import LLVM.General.Internal.Coding
import LLVM.General.Internal.EncodeAST
import LLVM.General.Internal.DecodeAST
import LLVM.General.Internal.Value ()

instance EncodeM EncodeAST String FFI.MDKindID where
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

instance DecodeM DecodeAST String FFI.MDKindID where
  decodeM (FFI.MDKindID k) = gets $ (Array.! (fromIntegral k)) . metadataKinds

instance DecodeM DecodeAST String (Ptr FFI.MDString) where
  decodeM p = do
    np <- alloca
    s <- liftIO $ FFI.getMDString p np
    n <- peek np
    decodeM (s, n)
