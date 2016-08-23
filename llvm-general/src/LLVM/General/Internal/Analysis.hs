module LLVM.General.Internal.Analysis where

import LLVM.General.Prelude

import Control.Monad.AnyCont
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Except

import qualified LLVM.General.Internal.FFI.Analysis as FFI
import qualified LLVM.General.Internal.FFI.LLVMCTypes as FFI

import LLVM.General.Internal.Module
import LLVM.General.Internal.Coding

-- | Run basic sanity checks on a 'Module'. Note that the same checks will trigger assertions
-- within LLVM if LLVM was built with them turned on, before this function can be is called.
verify :: Module -> ExceptT String IO ()
verify m = flip runAnyContT return $ do
  errorPtr <- alloca
  m' <- readModule m
  result <- decodeM =<< (liftIO $ FFI.verifyModule m' FFI.verifierFailureActionReturnStatus errorPtr)
  when result $ throwError =<< decodeM errorPtr

