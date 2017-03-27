module LLVM.Internal.Analysis where

import LLVM.Prelude

import Control.Monad.AnyCont
import Control.Monad.Catch
import Control.Monad.IO.Class

import qualified LLVM.Internal.FFI.Analysis as FFI
import qualified LLVM.Internal.FFI.LLVMCTypes as FFI

import LLVM.Internal.Module
import LLVM.Internal.Coding

import LLVM.Exception

-- | Run basic sanity checks on a 'Module'. Note that the same checks will trigger assertions
-- within LLVM if LLVM was built with them turned on, before this function can be is called.
verify :: Module -> IO ()
verify m = flip runAnyContT return $ do
  errorPtr <- alloca
  m' <- readModule m
  result <- decodeM =<< (liftIO $ FFI.verifyModule m' FFI.verifierFailureActionReturnStatus errorPtr)
  when result $ throwM . VerifyException =<< decodeM errorPtr
