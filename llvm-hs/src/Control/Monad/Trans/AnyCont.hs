{-# LANGUAGE
  CPP, RankNTypes
  #-}
module Control.Monad.Trans.AnyCont where

import LLVM.Prelude

import Control.Monad.Catch
import Control.Monad.Cont as Cont
import Control.Monad.Fail as Fail

newtype AnyContT m a = AnyContT { unAnyContT :: forall r . ContT r m a }

instance Functor (AnyContT m) where
  fmap f p = AnyContT $ fmap f (unAnyContT p)
  {-# INLINE fmap #-}

instance Applicative (AnyContT m) where
  pure a = AnyContT $ pure a
  {-# INLINE pure #-}
  f <*> v = AnyContT $ unAnyContT f <*> unAnyContT v
  {-# INLINE (<*>) #-}

instance Monad m => Monad (AnyContT m) where
  AnyContT f >>= k = AnyContT $ f >>= \x -> unAnyContT (k x)
  {-# INLINE (>>=) #-}
  return a = AnyContT $ return a
  {-# INLINE return #-}
#if !(MIN_VERSION_base(4,13,0))
  fail s = AnyContT (ContT (\_ -> Cont.fail s))
  {-# INLINE fail #-}
#endif

instance MonadFail m => MonadFail (AnyContT m) where
  fail s = AnyContT (ContT (\_ -> Fail.fail s))
  {-# INLINE fail #-}

instance MonadIO m => MonadIO (AnyContT m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance MonadTrans AnyContT where
  lift ma = AnyContT (lift ma)
  {-# INLINE lift #-}

instance MonadThrow m => MonadThrow (AnyContT m) where
  throwM = lift . throwM
  {-# INLINE throwM #-}

runAnyContT :: AnyContT m a -> forall r . (a -> m r) -> m r
runAnyContT k = runContT (unAnyContT k)
{-# INLINE runAnyContT #-}

runAnyContT' :: (a -> m r) -> AnyContT m a -> m r
runAnyContT' f k = runContT (unAnyContT k) f
{-# INLINE runAnyContT' #-}

anyContT :: (forall r . (a -> m r) -> m r) -> AnyContT m a
anyContT f = AnyContT (ContT f)
{-# INLINE anyContT #-}

withAnyContT :: (forall r . (b -> m r) -> (a -> m r)) -> AnyContT m a -> AnyContT m b
withAnyContT f m = anyContT $ runAnyContT m . f
{-# INLINE withAnyContT #-}

mapAnyContT :: (forall r . m r -> m r) -> AnyContT m a -> AnyContT m a
mapAnyContT f m = anyContT $ f . runAnyContT m
{-# INLINE mapAnyContT #-}
