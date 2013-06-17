module Control.Monad.Trans.Phased where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype PhasedT m a = PhasedT { unPhasedT :: m (Either (PhasedT m a) a) }

instance Functor m => Functor (PhasedT m) where
  fmap f = PhasedT . fmap (either (Left . fmap f) (Right . f)) . unPhasedT

instance (Functor m, Monad m) => Applicative (PhasedT m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (PhasedT m) where
  k >>= f = PhasedT $ unPhasedT k >>= either (return . Left . (>>= f)) (unPhasedT . f)
  return = PhasedT . return . Right
  fail = PhasedT . fail

instance MonadTrans PhasedT where
  lift = PhasedT . liftM Right

runPhasedT :: Monad m => PhasedT m a -> m a
runPhasedT = either runPhasedT return <=< unPhasedT

instance MonadIO m => MonadIO (PhasedT m) where
  liftIO = PhasedT . liftM Right . liftIO

mapPhasedT :: Monad n => (m (Either (PhasedT m a) a) -> n (Either (PhasedT m a) b)) -> PhasedT m a -> PhasedT n b
mapPhasedT f (PhasedT x) = PhasedT $ return (either (Left . (mapPhasedT f)) Right) `ap` f x

