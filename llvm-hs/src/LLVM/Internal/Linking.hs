module LLVM.Internal.Linking
  ( loadLibraryPermanently
  , getSymbolAddressInProcess
  )where

import LLVM.Prelude

import qualified Data.ByteString as BS
import Foreign.C.String
import Foreign.Ptr
import LLVM.Internal.Coding
import qualified LLVM.Internal.FFI.DynamicLibrary as DL
import qualified LLVM.Internal.FFI.RTDyldMemoryManager as Dyld
import LLVM.Internal.OrcJIT

-- | Get the address of the given symbol in
--   the current process' address space.
getSymbolAddressInProcess
  :: MangledSymbol -> IO WordPtr
getSymbolAddressInProcess (MangledSymbol sym)
  = fromIntegral <$> BS.useAsCString sym Dyld.getSymbolAddressInProcess

-- | Loads the given dynamic library permanently. If 'Nothing'
--   is given, this will make the symbols from the current
--   process available.
loadLibraryPermanently
  :: Maybe FilePath
  -> IO Bool
loadLibraryPermanently (Just fp) = decodeM =<< withCString fp DL.loadLibraryPermanently
loadLibraryPermanently Nothing = decodeM =<< DL.loadLibraryPermanently nullPtr
