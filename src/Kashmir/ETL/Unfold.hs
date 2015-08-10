module Kashmir.ETL.Unfold (unfoldrM) where

-- TODO Useful, but doesn't belong in this project.

-- | Monadic Unfold.
unfoldrM :: Monad m
         => (b -> m (Maybe (a,b))) -> b -> m [a]
unfoldrM f b =
  do r <- f b
     case r of
       Just (a,b') ->
         do as <- unfoldrM f b'
            return $ a : as
       Nothing -> return []
