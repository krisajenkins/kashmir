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
import           Web.HttpApiData
import           Web.PathPieces

newtype Email = Email
    { unEmail :: Text
    } deriving (Show, Read, Eq, Ord, Generic)

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

instance PathPiece Email where
    toPathPiece (Email t) = t
    fromPathPiece t =
        case parseEmail t of
            Left _ -> Nothing
            Right e -> Just e

instance ToHttpApiData Email where
    toUrlPiece (Email t) = t

instance FromHttpApiData Email where
    parseUrlPiece = parseEmail
