module LLVM.Internal.ObjectFile where

import Control.Exception (bracket)
import Control.Monad.IO.Class
import Control.Monad.AnyCont
import Foreign.Ptr
import GHC.Stack

import LLVM.Prelude
import LLVM.Internal.Coding
import LLVM.Internal.MemoryBuffer
import qualified LLVM.Internal.FFI.LLVMCTypes as FFI
import qualified LLVM.Internal.FFI.ObjectFile as FFI

newtype ObjectFile = ObjectFile (Ptr FFI.ObjectFile)

-- | Dispose of an 'ObjectFile'.
disposeObjectFile :: ObjectFile -> IO ()
disposeObjectFile (ObjectFile obj) = FFI.disposeObjectFile obj

-- | Create a object file which can later be linked with the
-- 'LLVM.Internal.OrcJIT.LinkingLayer'.
--
-- Note that the file at `path` should already be a compiled object file i.e a
-- `.o` file. This does *not* compile source files.
createObjectFile :: HasCallStack => FilePath -> IO ObjectFile
createObjectFile path = flip runAnyContT return $ do
  -- The ownership of the object file is transfered to the object
  -- file, so we need to make sure that we do not free it here.
  FFI.OwnerTransfered buf <- encodeM (File path)
  obj <- liftIO $ FFI.createObjectFile buf
  when (obj == nullPtr) $ error "LLVMCreateObjectFile returned a null pointer."
  return (ObjectFile obj)

-- | @bracket@-style wrapper for `createObjectFile` and `disposeObjectFile`.
withObjectFile :: FilePath -> (ObjectFile -> IO a) -> IO a
withObjectFile f = bracket (createObjectFile f) disposeObjectFile
