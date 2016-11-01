{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Kashmir.Email
  ( Email()
  , unEmail
  , parseEmail
  ) where

import           Data.Aeson
import           Data.Text
import           Database.Persist
import           Database.Persist.Sql
import           GHC.Generics

newtype Email = Email
    { unEmail :: Text
    } deriving (Show, Read, Eq, Generic)

parseEmail :: Text -> Either Text Email
parseEmail t = Right (Email t)

instance PersistField Email where
    toPersistValue (Email t) = toPersistValue t
    fromPersistValue t = Email <$> fromPersistValue t

instance PersistFieldSql Email where
    sqlType _ = SqlOther "text"

instance ToJSON Email where
    toJSON (Email t) = toJSON t

instance FromJSON Email where
    parseJSON t = Email <$> parseJSON t
