module Kashmir.ETL.Pipes (unfoldrPipe,flattenPipe,toBinary) where

import           Data.ByteString
import           Data.Text       ()
import           Pipes
import           Pipes.Binary

unfoldrPipe :: Monad m
            => (b -> m (Maybe (a,b))) -> b -> Producer a m ()
unfoldrPipe f b =
  do r <- lift $ f b
     case r of
       Just (a,b') ->
         yield a >>
         unfoldrPipe f b'
       Nothing -> return ()

flattenPipe :: Monad m => Pipe [a] a m ()
flattenPipe =
  do xs <- await
     _ <- mapM yield xs
     flattenPipe

toBinary :: (Monad m, Binary b) => Pipe b ByteString m ()
toBinary = for cat encode
