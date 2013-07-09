module LLVM.General.Internal.Analysis where

import Control.Monad.Error
import Control.Monad.AnyCont

import qualified LLVM.General.Internal.FFI.Analysis as FFI
import qualified LLVM.General.Internal.FFI.LLVMCTypes as FFI

import LLVM.General.Internal.Module
import LLVM.General.Internal.Coding

-- | Run basic sanity checks on a 'Module'. Note that the same checks will trigger assertions
-- within LLVM if LLVM was built with them turned on, before this function can be is called.
verify :: Module -> ErrorT String IO ()
verify (Module m) = flip runAnyContT return $ do
  errorPtr <- alloca
  result <- decodeM =<< (liftIO $ FFI.verifyModule m FFI.verifierFailureActionReturnStatus errorPtr)
  when result $ fail =<< decodeM errorPtr

