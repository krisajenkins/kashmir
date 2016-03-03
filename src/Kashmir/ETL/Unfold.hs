module Kashmir.ETL.Unfold (unfoldrM) where

import           Data.Monoid
-- TODO Useful, but doesn't belong in this project.

-- | Monadic Unfold.
unfoldrM :: (Monad m,Monoid r)
         => (b -> m (Maybe (r,b))) -> b -> m r
unfoldrM f b =
  do r <- f b
     case r of
       Just (a,b') ->
         do as <- unfoldrM f b'
            return $ a <> as
       Nothing -> return mempty
