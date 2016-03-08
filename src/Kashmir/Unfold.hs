module Kashmir.Unfold (unfoldrM) where

-- | Monadic Unfold.
unfoldrM :: (Monad m,Monoid a)
         => (b -> m (Maybe (a,b))) -> b -> m a
unfoldrM f seed =
  do result <- f seed
     case result of
       Nothing -> return mempty
       Just (page,seed') -> mappend page <$> unfoldrM f seed'
