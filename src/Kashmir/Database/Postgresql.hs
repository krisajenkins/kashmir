{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Kashmir.Database.Postgresql
       (insertUnlessDuplicate, upsert, runSql, trim_, array_,
        connectionDetails, regexpReplace_, DbConfig(..), connectionString,
        poolSize)
       where

import           Control.Exception.Lifted
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Aeson                      hiding (Value)
import qualified Data.ByteString.Char8           as BC
import           Data.Maybe
import           Data.String
import           Data.Text                       ()
import           Database.Esqueleto              hiding (update, upsert)
import           Database.Esqueleto.Internal.Sql
import           Database.Persist.Postgresql     (ConnectionString,
                                                  createPostgresqlPool)
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           Kashmir.Aeson
import           Prelude                         hiding (product)

data DbConfig =
  DbConfig {_connectionString :: String
           ,_poolSize         :: Int}
  deriving (Eq,Show,Generic)
makeLenses ''DbConfig

instance ToJSON DbConfig where
  toJSON = genericToJSON $ dropPrefixJSONOptions "_"
instance FromJSON DbConfig where
  parseJSON = genericParseJSON $ dropPrefixJSONOptions "_"

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

connectionDetails :: DbConfig -> ConnectionString
connectionDetails = BC.pack . view connectionString

runSql :: DbConfig -> SqlPersistM a -> IO a
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
trim_ pattern target =
  unsafeSqlFunction "trim"
                    (unsafeSqlBinOp "FROM" pattern target)

regexpReplace_ :: (IsString s)
               => SqlExpr (Value s) -> SqlExpr (Value s) -> SqlExpr (Value s) -> SqlExpr (Value s) -> SqlExpr (Value s)
regexpReplace_ string pattern replacement flags =
  unsafeSqlFunction "regexp_replace"
                    (string,pattern,replacement,flags)
