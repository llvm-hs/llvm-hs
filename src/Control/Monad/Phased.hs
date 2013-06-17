{-# LANGUAGE
  FlexibleInstances,
  MultiParamTypeClasses,
  UndecidableInstances
  #-}
module Control.Monad.Phased (
  MonadPhased(..),
  PhasedT(..),
  runPhasedT,
  forInterleavedM,
  iap,
  defer,
  runInterleaved,
  mapPhasedT
  ) where

import Control.Monad
import Control.Monad.Trans.Phased

import Control.Monad.Trans.Class
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Control.Monad.Error.Class
import Control.Monad.Phased.Class

instance MonadState s m => MonadState s (PhasedT m) where
  state = lift . state

instance MonadReader r m => MonadReader r (PhasedT m) where
  ask = lift ask
  local f = PhasedT . local f . liftM (either (Left . local f) Right) . unPhasedT

instance (MonadError e m) => MonadError e (PhasedT m) where
  throwError = lift . throwError
  catchError pa ph = mapPhasedT (`catchError` (unPhasedT . ph)) pa
  
