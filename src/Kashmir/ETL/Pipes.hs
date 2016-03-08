module Kashmir.ETL.Pipes (unfoldrPipe,flattenPipe,toBinary) where

import           Data.ByteString
import           Data.Text       ()
import           Pipes
import           Pipes.Binary

unfoldrPipe :: Monad m
            => (b -> m (Maybe (a,b))) -> b -> Producer a m ()
unfoldrPipe f seed =
  do result <- lift $ f seed
     case result of
       Nothing -> return mempty
       Just (page,seed') -> yield page >> unfoldrPipe f seed'

flattenPipe :: Monad m => Pipe [a] a m ()
flattenPipe =
  do xs <- await
     _ <- mapM yield xs
     flattenPipe

toBinary :: (Monad m, Binary b) => Pipe b ByteString m ()
toBinary = for cat encode
