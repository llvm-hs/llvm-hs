{-# LANGUAGE
  MultiParamTypeClasses,
  UndecidableInstances,
  FlexibleInstances,
  TupleSections
  #-}
module Control.Monad.Phased.Class where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Phased
import Control.Monad.Trans.AnyCont

class Monad m => MonadPhased m where
  later :: m a -> m a
  interleavePhasesWith :: (a -> b -> c) -> m a -> m b -> m c
  mergePhases :: m a -> m a

iap :: MonadPhased m => m (a -> b) -> m a -> m b
iap = interleavePhasesWith ($)

runInterleaved :: (MonadPhased m) => [m a] -> m [a]
runInterleaved = foldr (interleavePhasesWith (:)) (return [])

forInterleavedM x = runInterleaved . flip map x

defer :: MonadPhased m => m ()
defer = later (return ())

instance Monad m => MonadPhased (PhasedT m) where
  later = PhasedT . return . Left
  interleavePhasesWith p (PhasedT mx) (PhasedT my) = PhasedT $ do
    x <- mx
    y <- my
    return $ case (x,y) of
      (Right a, Right b) -> Right (p a b)
      _ -> Left $ interleavePhasesWith p (stall x) (stall y)
           where stall = either id return
  mergePhases = lift . runPhasedT

instance MonadPhased m => MonadPhased (AnyContT m) where
  later = lift . later . flip runAnyContT return
  interleavePhasesWith p mx my = 
    anyContT $ (>>=) $ interleavePhasesWith p (runAnyContT mx return) (runAnyContT my return)
  mergePhases ma = anyContT (mergePhases (runAnyContT ma return) >>= )


