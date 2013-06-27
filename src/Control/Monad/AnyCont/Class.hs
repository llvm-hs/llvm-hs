{-# LANGUAGE
  RankNTypes,
  MultiParamTypeClasses,
  FunctionalDependencies,
  FlexibleInstances,
  UndecidableInstances
  #-}
module Control.Monad.AnyCont.Class where

import Control.Monad.Trans.Class
import Control.Monad.Trans.AnyCont (AnyContT)
import qualified Control.Monad.Trans.AnyCont as AnyCont
import Control.Monad.Trans.Error as Error
import Control.Monad.Trans.State as State
import Control.Monad.Trans.Phased as Phased

class MonadAnyCont b m | m -> b where
  anyContToM :: (forall r . (a -> b r) -> b r) -> m a
  scopeAnyCont :: m a -> m a

instance Monad m => MonadAnyCont m (AnyContT m) where
  anyContToM = AnyCont.anyContT
  scopeAnyCont = lift . flip AnyCont.runAnyContT return
                                     
instance (Error e, Monad m, MonadAnyCont b m) => MonadAnyCont b (ErrorT e m) where
  anyContToM = lift . anyContToM
  scopeAnyCont = mapErrorT scopeAnyCont

instance (Monad m, MonadAnyCont b m) => MonadAnyCont b (PhasedT m) where
  anyContToM = lift . anyContToM
  scopeAnyCont = mapPhasedT scopeAnyCont

instance (Monad m, MonadAnyCont b m) => MonadAnyCont b (StateT s m) where
  anyContToM = lift . anyContToM
  scopeAnyCont = StateT . (scopeAnyCont .) . runStateT

class LiftAnyCont b m where
  liftAnyCont :: (forall r . (a -> b r) -> b r) -> (forall r . (a -> m r) -> m r)

instance LiftAnyCont b b where
  liftAnyCont c = c

instance LiftAnyCont b m => LiftAnyCont b (PhasedT m) where
  liftAnyCont c = \q -> PhasedT (liftAnyCont c (unPhasedT . q))

instance LiftAnyCont b m => LiftAnyCont b (StateT s m) where
  liftAnyCont c = \q -> StateT $ \s -> (liftAnyCont c (($ s) . runStateT . q))

instance LiftAnyCont b m => LiftAnyCont b (ErrorT e m) where
  liftAnyCont c = \q -> ErrorT (liftAnyCont c (runErrorT . q))