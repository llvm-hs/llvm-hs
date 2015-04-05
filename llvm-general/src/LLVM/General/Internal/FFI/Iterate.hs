-- | Functions to help handle LLVM iteration patterns
module LLVM.General.Internal.FFI.Iterate where

import LLVM.General.Prelude

import Foreign.Ptr

-- | retrieve a sequence of objects which form a linked list, given an action to
-- | retrieve the first member and an action to proceed through the list
getXs :: IO (Ptr a) -> (Ptr a -> IO (Ptr a)) -> IO [Ptr a]
getXs firstX nextX = walk =<< firstX
    where walk x | x == nullPtr = return []
          walk x = (x:) <$> (walk <=< nextX) x
