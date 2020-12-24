module LLVM.Internal.Context where

import LLVM.Prelude

import Control.Exception
import Control.Concurrent

import Foreign.Ptr

import qualified LLVM.Internal.FFI.Context as FFI

-- | a Context object holds the state the of LLVM system needs for one thread of
-- | LLVM compilation. Once upon a time, in early versions of LLVM, this state was global.
-- | Then it got packed up in this object to allow multiple threads to compile at once.
data Context = Context (Ptr FFI.Context)

-- | Create a Context, run an action (to which it is provided), then destroy the Context.
withContext :: (Context -> IO a) -> IO a
withContext = runBound . bracket FFI.contextCreate FFI.contextDispose . (. Context)
  where runBound = if rtsSupportsBoundThreads then runInBoundThread else id

-- | Create a Context.
createContext :: IO Context
createContext = Context <$> FFI.contextCreate

-- | Destroy a context created by 'createContext'.
disposeContext :: Context -> IO ()
disposeContext (Context ptr) = FFI.contextDispose ptr
