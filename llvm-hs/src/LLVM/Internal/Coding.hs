{-# LANGUAGE
  TemplateHaskell,
  MultiParamTypeClasses,
  FunctionalDependencies,
  UndecidableInstances
  #-}
module LLVM.Internal.Coding where

import LLVM.Prelude

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Control.Monad.AnyCont
import Control.Monad.IO.Class

import Foreign.C
import Foreign.Ptr
import Foreign.Storable (Storable)
import qualified Foreign.Storable
import qualified Foreign.Marshal.Alloc
import qualified Foreign.Marshal.Array
import GHC.Stack

import qualified LLVM.Internal.FFI.LLVMCTypes as FFI

class EncodeM e h c where
  encodeM :: HasCallStack => h -> e c

class DecodeM d h c where
  decodeM :: HasCallStack => c -> d h

genCodingInstance :: (Data c, Data h) => TypeQ -> Name -> [(c, h)] -> Q [Dec]
genCodingInstance ht ctn chs = do
  let n = const Nothing
  [d| 
    instance Monad m => EncodeM m $(ht) $(conT ctn) where
      encodeM h = return $ $(
        caseE [| h |] [ match (dataToPatQ n h) (normalB (dataToExpQ n c)) [] | (c,h) <- chs ] 
       )

    instance Monad m => DecodeM m $(ht) $(conT ctn) where
      decodeM c = return $ $(
        caseE [| c |] ([ match (dataToPatQ n c) (normalB (dataToExpQ n h)) [] | (c,h) <- chs] ++
                       [ match wildP (normalB [e| error ("Decoding failed: Unknown " <> show c) |]) []]))
   |]

allocaArray :: (Integral i, Storable a, MonadAnyCont IO m) => i -> m (Ptr a)
allocaArray p = anyContToM $ Foreign.Marshal.Array.allocaArray (fromIntegral p)

alloca :: (Storable a, MonadAnyCont IO m) => m (Ptr a)
alloca = anyContToM Foreign.Marshal.Alloc.alloca

peek :: (Storable a, MonadIO m) => Ptr a -> m a
peek p = liftIO $ Foreign.Storable.peek p

peekByteOff :: (Storable a, MonadIO m) => Ptr a -> Int -> m a
peekByteOff p i = liftIO $ Foreign.Storable.peekByteOff p i

poke :: (Storable a, MonadIO m) => Ptr a -> a -> m ()
poke p a = liftIO $ Foreign.Storable.poke p a

pokeByteOff :: (Storable a, MonadIO m) => Ptr a -> Int -> a -> m ()
pokeByteOff p i a = liftIO $ Foreign.Storable.pokeByteOff p i a

peekArray :: (Integral i, Storable a, MonadIO m) => i -> Ptr a -> m [a]
peekArray n p = liftIO $ Foreign.Marshal.Array.peekArray (fromIntegral n) p

instance (Monad m, EncodeM m h c, Storable c, MonadAnyCont IO m) => EncodeM m [h] (CUInt, Ptr c) where
  encodeM hs = do
    hs <- mapM encodeM hs
    (anyContToM $ \x -> Foreign.Marshal.Array.withArrayLen hs $ \n hs -> x (fromIntegral n, hs))

instance (Monad m, DecodeM m h c, Storable c, MonadIO m) => DecodeM m [h] (CUInt, Ptr c) where
  decodeM (n, ca) = do
    cs <- liftIO $ Foreign.Marshal.Array.peekArray (fromIntegral n) ca
    mapM decodeM cs

instance Monad m => EncodeM m Bool FFI.LLVMBool where
  encodeM False = return $ FFI.LLVMBool 0
  encodeM True = return $ FFI.LLVMBool 1

instance Monad m => DecodeM m Bool FFI.LLVMBool where
  decodeM (FFI.LLVMBool 0) = return $ False
  decodeM (FFI.LLVMBool _) = return $ True

instance (Monad m, EncodeM m h (Ptr c)) => EncodeM m (Maybe h) (Ptr c) where
  encodeM = maybe (return nullPtr) encodeM

instance (Monad m, DecodeM m h (Ptr c)) => DecodeM m (Maybe h) (Ptr c) where
  decodeM p | p == nullPtr = return Nothing
            | otherwise = liftM Just $ decodeM p

instance Monad m => EncodeM m (Maybe Bool) (FFI.NothingAsMinusOne Bool) where
  encodeM = return . FFI.NothingAsMinusOne . maybe (-1) (fromIntegral . fromEnum)

instance Monad m => EncodeM m (Maybe Word) (FFI.NothingAsMinusOne Word) where
  encodeM = return . FFI.NothingAsMinusOne . maybe (-1) fromIntegral

instance Monad m => EncodeM m (Maybe Word32) (CUInt, FFI.LLVMBool) where
  encodeM (Just a) = liftM2 (,) (encodeM a) (encodeM True)
  encodeM Nothing = (0,) <$> encodeM False

instance Monad m => EncodeM m (Maybe Word32) (Word32, FFI.LLVMBool) where
  encodeM (Just a) = (a,) <$> encodeM True
  encodeM Nothing = (0,) <$> encodeM False

instance Monad m => EncodeM m Word CUInt where
  encodeM = return . fromIntegral

instance Monad m => EncodeM m Word32 CUInt where
  encodeM = return . fromIntegral

instance Monad m => EncodeM m Word64 CULong where
  encodeM = return . fromIntegral

instance Monad m => DecodeM m Word CUInt where
  decodeM = return . fromIntegral

instance Monad m => DecodeM m Word32 CUInt where
  decodeM = return . fromIntegral

instance Monad m => DecodeM m Word64 CULong where
  decodeM = return . fromIntegral

instance Monad m => EncodeM m Int32 CInt where
  encodeM = return . fromIntegral

instance Monad m => DecodeM m Int32 CInt where
  decodeM = return . fromIntegral

instance Monad m => DecodeM m Int CInt where
  decodeM = return . fromIntegral

instance Monad m => EncodeM m Word64 Word64 where
  encodeM = return

instance Monad m => DecodeM m Word64 Word64 where
  decodeM = return

decodeOptional :: (DecodeM m b a, Storable a, MonadAnyCont IO m, MonadIO m) => (Ptr a -> IO FFI.LLVMBool) -> m (Maybe b)
decodeOptional f = do
  ptr <- alloca
  isJust <- decodeM =<< liftIO (f ptr)
  if isJust
    then Just <$> (decodeM =<< peek ptr)
    else pure Nothing

decodeArray :: (DecodeM m b' b, MonadIO m) => (a -> IO CUInt) -> (a -> CUInt -> IO b) -> a -> m [b']
decodeArray numElems getElem a = do
  n <- liftIO (numElems a)
  if n == 0
    then pure []
    else traverse (decodeM <=< liftIO . getElem a) [0 .. n - 1]
