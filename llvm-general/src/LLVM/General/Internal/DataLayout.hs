module LLVM.General.Internal.DataLayout where

import LLVM.General.Prelude

import Control.Monad.Exceptable
import Control.Monad.AnyCont
import Control.Exception

import Foreign.Ptr

import qualified LLVM.General.Internal.FFI.DataLayout as FFI

import LLVM.General.AST.DataLayout
import LLVM.General.DataLayout

import LLVM.General.Internal.Coding
import LLVM.General.Internal.String ()

withFFIDataLayout :: DataLayout -> (Ptr FFI.DataLayout -> IO a) -> IO a
withFFIDataLayout dl f = flip runAnyContT return $ do
  dls <- encodeM (dataLayoutToString dl)
  liftIO $ bracket (FFI.createDataLayout dls) FFI.disposeDataLayout f

