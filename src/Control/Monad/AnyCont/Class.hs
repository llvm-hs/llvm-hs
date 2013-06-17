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
import Control.Monad.IO.Class

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

anyContIOToM :: MonadIO m => (forall r . (a -> IO r) -> IO r) -> AnyContT m a
anyContIOToM ioac = AnyCont.anyContT (\c -> liftIO (ioac return) >>= c)
