module LLVM.Internal.DataLayout where

import LLVM.Prelude

import Control.Exception
import Control.Monad.AnyCont
import Control.Monad.IO.Class

import Foreign.Ptr

import qualified LLVM.Internal.FFI.DataLayout as FFI

import LLVM.AST.DataLayout
import LLVM.DataLayout

import LLVM.Internal.Coding
import LLVM.Internal.String ()

withFFIDataLayout :: DataLayout -> (Ptr FFI.DataLayout -> IO a) -> IO a
withFFIDataLayout dl f = flip runAnyContT return $ do
  dls <- encodeM (dataLayoutToString dl)
  liftIO $ bracket (FFI.createDataLayout dls) FFI.disposeDataLayout f

