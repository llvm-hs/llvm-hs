{-# LANGUAGE
  RankNTypes,
  MultiParamTypeClasses,
  UndecidableInstances
  #-}
module Control.Monad.AnyCont.Class where

import Prelude

import Control.Monad.Trans.Class
import Control.Monad.Trans.AnyCont (AnyContT)
import qualified Control.Monad.Trans.AnyCont as AnyCont
import Control.Monad.Trans.Except as Except
import Control.Monad.Trans.State as State

class ScopeAnyCont m where
  scopeAnyCont :: m a -> m a

class MonadAnyCont b m where
  anyContToM :: (forall r . (a -> b r) -> b r) -> m a


instance MonadTransAnyCont b m => MonadAnyCont b (AnyContT m) where
  anyContToM c = AnyCont.anyContT (liftAnyCont c)

instance Monad m => ScopeAnyCont (AnyContT m) where
  scopeAnyCont = lift . flip AnyCont.runAnyContT return


instance (Monad m, MonadAnyCont b m) => MonadAnyCont b (StateT s m) where
  anyContToM x = lift $ anyContToM x

instance ScopeAnyCont m => ScopeAnyCont (StateT s m) where
  scopeAnyCont = StateT . (scopeAnyCont .) . runStateT


instance (Monad m, MonadAnyCont b m) => MonadAnyCont b (ExceptT e m) where
  anyContToM x = lift $ anyContToM x


instance ScopeAnyCont m => ScopeAnyCont (ExceptT e m) where
  scopeAnyCont = mapExceptT scopeAnyCont

class MonadTransAnyCont b m where
  liftAnyCont :: (forall r . (a -> b r) -> b r) -> (forall r . (a -> m r) -> m r)

instance MonadTransAnyCont b b where
  liftAnyCont c = c

instance MonadTransAnyCont b m => MonadTransAnyCont b (StateT s m) where
  liftAnyCont c = (\c q -> StateT $ \s -> c $ ($ s) . runStateT . q) (liftAnyCont c)

instance MonadTransAnyCont b m => MonadTransAnyCont b (ExceptT e m) where
  liftAnyCont c = (\c q -> ExceptT . c $ runExceptT . q) (liftAnyCont c)
