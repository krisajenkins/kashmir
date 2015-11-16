{-# LANGUAGE FlexibleContexts #-}
module Kashmir.Snap.Snaplet.Postgresql where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.State.Class
import qualified Data.ByteString.Char8       as C
import           Database.Esqueleto          hiding (get)
import           Database.Persist            ()
import           Database.Persist.Postgresql (createPostgresqlPool)
import           Kashmir.Database.Postgresql
import           Snap

initDb :: DatabaseConfig -> SnapletInit a ConnectionPool
initDb config =
  makeSnaplet "connection-pool" "A simple Postgresql connection pool" Nothing .
  liftIO . runStdoutLoggingT $
  createPostgresqlPool (C.pack (view connectionString config))
                       (view poolSize config)

sqlHandler :: (MonadIO m,MonadState ConnectionPool m)
           => SqlPersistM a -> m a
sqlHandler query =
  do connection <- get
     liftIO $
       runSqlPersistMPool query connection
