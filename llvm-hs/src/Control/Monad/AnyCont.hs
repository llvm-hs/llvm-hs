{-# LANGUAGE
  MultiParamTypeClasses,
  UndecidableInstances
  #-}
module Control.Monad.AnyCont (
    MonadAnyCont(..),
    ScopeAnyCont(..),
    AnyContT(..),
    MonadTransAnyCont(..),
    runAnyContT, runAnyContT',
    withAnyContT,
    mapAnyContT
  ) where

import Prelude

import Control.Monad.Trans.AnyCont
import Control.Monad.AnyCont.Class
import Control.Monad.Trans.Class
import Control.Monad.State.Class
import Control.Monad.Error.Class

instance MonadState s m => MonadState s (AnyContT m) where
  get = lift get
  {-# INLINE get #-}
  put = lift . put
  {-# INLINE put #-}
  state = lift . state
  {-# INLINE state #-}

instance MonadError e m => MonadError e (AnyContT m) where
  throwError = lift . throwError
  {-# INLINE throwError #-}
  x `catchError` h = anyContT $ \f -> (runAnyContT x f) `catchError` (\e -> runAnyContT (h e) f)
