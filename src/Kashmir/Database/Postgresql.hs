{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Kashmir.Database.Postgresql
       (insertUnlessDuplicate, upsert, runSql, trim_, array_,
        connectionDetails, regexpReplace_, DatabaseConfig(..),
        connectionString, createSavepoint, releaseSavepoint,
        rollbackToSavepoint, poolSize,
        toTsquery, toTsvector, (@@.))
       where

import           Control.Exception.Lifted
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Aeson.TH                   (deriveJSON)
import qualified Data.ByteString.Char8           as BC
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text                       (Text)
import           Database.Esqueleto              hiding (update, upsert)
import           Database.Esqueleto.Internal.Sql
import           Database.Persist.Postgresql     (ConnectionString,
                                                  createPostgresqlPool)
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           Kashmir.Aeson
import           Prelude                         hiding (product)

-- TODO Should we make the connectionString a ByteString? Text? IsString?
data DatabaseConfig =
  DatabaseConfig {_connectionString :: String
                 ,_poolSize         :: Int}
  deriving (Eq,Show,Generic)
makeLenses ''DatabaseConfig
$(deriveJSON (dropPrefixJSONOptions "_") ''DatabaseConfig)

sqlErrorCode :: SqlError -> String
sqlErrorCode = BC.unpack . sqlState

insertUnlessDuplicate :: (PersistEntity a,PersistEntityBackend a ~ SqlBackend)
                      => a -> SqlPersistM (Maybe (Key a))
insertUnlessDuplicate item =
  rawExecute "SAVEPOINT guard_duplicate" [] >>
  try (insert item) >>=
  ignoreDuplicateError
  where ignoreDuplicateError :: MonadIO m
                             => Either SqlError a -> SqlPersistT m (Maybe a)
        ignoreDuplicateError (Right x) =
          rawExecute "RELEASE SAVEPOINT guard_duplicate" [] >>
          (return . Just) x
        ignoreDuplicateError (Left e)
          | sqlErrorCode e == "23505" =
            rawExecute "ROLLBACK TO SAVEPOINT guard_duplicate" [] >>
            return Nothing
          | otherwise = fail (show e)

upsert :: (PersistEntity a,PersistEntityBackend a ~ SqlBackend)
       => (a -> Key a) -> a -> SqlPersistM ()
upsert keyFunction item =
  rawExecute "SAVEPOINT guard_duplicate" [] >>
  try (insert item) >>=
  onDuplicateError
  where onDuplicateError :: MonadIO m
                         => Either SqlError a -> SqlPersistT m ()
        onDuplicateError (Right _) =
          rawExecute "RELEASE SAVEPOINT guard_duplicate" [] >>
          return ()
        onDuplicateError (Left e)
          | sqlErrorCode e == "23505" =
            do _ <-
                 rawExecute "ROLLBACK TO SAVEPOINT guard_duplicate" []
               replace (keyFunction item) item
          | otherwise = fail (show e)

------------------------------------------------------------

connectionDetails :: DatabaseConfig -> ConnectionString
connectionDetails = BC.pack . view connectionString

runSql :: DatabaseConfig -> SqlPersistM a -> IO a
runSql dbConfig sql =
  do pool <-
       runNoLoggingT
         (createPostgresqlPool (connectionDetails dbConfig)
                               (view poolSize dbConfig))
     runSqlPersistMPool sql pool

------------------------------------------------------------

array_ :: IsString s
       => SqlExpr (Value s) -> SqlExpr (Value [s])
array_ = unsafeSqlFunction "array"

trim_ :: IsString s
      => SqlExpr (Value s) -> SqlExpr (Value s) -> SqlExpr (Value s)
trim_ regex target =
  unsafeSqlFunction "trim"
                    (unsafeSqlBinOp "FROM" regex target)

regexpReplace_ :: (IsString s)
               => SqlExpr (Value s) -> SqlExpr (Value s) -> SqlExpr (Value s) -> SqlExpr (Value s) -> SqlExpr (Value s)
regexpReplace_ string regex replacement flags =
  unsafeSqlFunction "regexp_replace"
                    (string,regex,replacement,flags)

------------------------------------------------------------

createSavepoint :: Text -> SqlPersistM ()
createSavepoint savepointName =
  rawExecute ("SAVEPOINT " <> savepointName)
             []


releaseSavepoint :: Text -> SqlPersistM ()
releaseSavepoint savepointName =
  rawExecute ("RELEASE SAVEPOINT " <> savepointName)
             []

rollbackToSavepoint :: Text -> SqlPersistM ()
rollbackToSavepoint savepointName =
  rawExecute ("ROLLBACK TO SAVEPOINT " <> savepointName)
             []

------------------------------------------------------------
-- Full Text Search

data TSQuery
data TSVector

toTsquery :: IsString s
          => SqlExpr (Value s) -> SqlExpr (Value TSQuery)
toTsquery = unsafeSqlFunction "to_tsquery"

toTsvector :: IsString s => SqlExpr (Value s) -> SqlExpr (Value TSVector)
toTsvector = unsafeSqlFunction "to_tsvector"

match :: SqlExpr (Value TSVector) -> SqlExpr (Value TSQuery) -> SqlExpr (Value Bool)
match = unsafeSqlBinOp " @@ "

(@@.) :: SqlExpr (Value TSVector) -> SqlExpr (Value TSQuery) -> SqlExpr (Value Bool)
(@@.) = match
