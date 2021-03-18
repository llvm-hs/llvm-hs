module LLVM.Internal.Linking
  ( loadLibraryPermanently
  , getSymbolAddressInProcess
  )where

import LLVM.Prelude

import Foreign.C.String
import Foreign.Ptr
import LLVM.Internal.Coding
import LLVM.Internal.OrcJIT
import qualified LLVM.Internal.FFI.OrcJIT as FFI
import qualified LLVM.Internal.FFI.DynamicLibrary as DL
import qualified LLVM.Internal.FFI.RTDyldMemoryManager as Dyld

-- | Get the address of the given symbol in
--   the current process' address space.
getSymbolAddressInProcess
  :: MangledSymbol -> IO WordPtr
getSymbolAddressInProcess (MangledSymbol sym) = do
  symStr <- FFI.mangledSymbolString sym
  fromIntegral <$> Dyld.getSymbolAddressInProcess symStr

-- | Loads the given dynamic library permanently. If 'Nothing'
--   is given, this will make the symbols from the current
--   process available.
loadLibraryPermanently
  :: Maybe FilePath
  -> IO Bool
loadLibraryPermanently (Just fp) = decodeM =<< withCString fp DL.loadLibraryPermanently
loadLibraryPermanently Nothing = decodeM =<< DL.loadLibraryPermanently nullPtr
