module LLVM.General.Internal.Threading (
  setMultithreaded,
  isMultithreaded
  ) where

import LLVM.General.Prelude

import Control.Concurrent.MVar
import System.IO.Unsafe

import qualified LLVM.General.Internal.FFI.Threading as FFI

import LLVM.General.Internal.Coding

lock :: MVar ()
{-# NOINLINE lock #-}
lock = unsafePerformIO $ newMVar ()

-- | Set the multithreading mode of LLVM. If it is disabled (as by default) locks are not enforced.
setMultithreaded :: Bool -> IO ()
setMultithreaded s = do
  takeMVar lock
  multi <- isMultithreaded
  case (s,multi) of
    (True,False) -> do
      success <- decodeM =<< FFI.startMultithreaded
      when (not success) $ error "setMultithreaded: LLVM not built with threading support"
    (False,True) -> FFI.stopMultithreaded
    _            -> return ()
  putMVar lock ()

-- | Check if multithreading is enabled in LLVM
isMultithreaded :: IO Bool
isMultithreaded = FFI.isMultithreaded >>= decodeM
