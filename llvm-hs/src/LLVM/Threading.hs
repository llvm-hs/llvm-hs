-- | functionality necessary when running LLVM in multiple threads at the same time.
module LLVM.Threading (
  setMultithreaded,
  isMultithreaded
  ) where

import LLVM.Prelude

import LLVM.Internal.Threading

{-# DEPRECATED setMultithreaded "LLVM no longer features runtime control of multithreading support" #-}
-- | This function used set the multithreading mode of LLVM, but that feature no longer exists. Threading is
-- controlled only at runtime with the configure flag --enable-threads (default is YES). This function will
-- now check that the the compiled-in multithreading support (returned by 'isMultithreaded') is
-- sufficient to support the requested access, and fail if not, so as to prevent uncontrolled use of a
-- version of LLVM compiled to be capable only of singled threaded use by haskell code requesting
-- multithreading support.
setMultithreaded :: Bool -> IO ()
setMultithreaded desired = do
  actual <- isMultithreaded
  when (desired && not actual) $
     fail $ "Multithreading support requested but not available. " ++ 
            "Please use an LLVM built with threading enabled"
  return ()
