module LLVM.Internal.FFI.ShortByteString
  ( packCString
  , packCStringLen
  , useAsCString
  , useAsCStringLen
  ) where

import LLVM.Prelude

import Data.ByteString.Internal (c_strlen)
import Data.ByteString.Short.Internal (createFromPtr, copyToPtr)
import qualified Data.ByteString.Short as ByteString
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable

{-# INLINABLE packCString #-}
packCString :: CString -> IO ShortByteString
packCString cstr = do
    len <- c_strlen cstr
    packCStringLen (cstr, fromIntegral len)

{-# INLINABLE packCStringLen #-}
packCStringLen :: CStringLen -> IO ShortByteString
packCStringLen (cstr, len) | len >= 0 = createFromPtr cstr len
packCStringLen (_, len) =
    error ("negative length: " ++ show len)

{-# INLINABLE useAsCString #-}
useAsCString :: ShortByteString -> (CString -> IO a) -> IO a
useAsCString bs action =
 allocaBytes (l+1) $ \buf -> do
     copyToPtr bs 0 buf (fromIntegral l)
     pokeByteOff buf l (0::Word8)
     action buf
 where l = ByteString.length bs

{-# INLINABLE useAsCStringLen #-}
useAsCStringLen :: ShortByteString -> (CStringLen -> IO a) -> IO a
useAsCStringLen bs action =
 allocaBytes l $ \buf -> do
     copyToPtr bs 0 buf (fromIntegral l)
     action (buf, l)
 where l = ByteString.length bs
