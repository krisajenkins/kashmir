{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans #-}
module Kashmir.UUID (toStrictByteString, module X) where

import           Data.Aeson
import           Data.ByteString      (ByteString)

import           Data.Monoid
import           Data.Text
import           Data.Text.Encoding   (decodeUtf8, encodeUtf8)
import           Data.UUID            as X
import           Database.Persist
import           Database.Persist.Sql
import           Web.HttpApiData
import           Web.PathPieces

instance PersistField UUID where
  toPersistValue :: UUID -> PersistValue
  toPersistValue = PersistDbSpecific . toStrictByteString
  fromPersistValue
    :: PersistValue -> Either Text UUID
  fromPersistValue (PersistDbSpecific s) =
    case (fromText . decodeUtf8) s of
      Nothing -> Left $ mconcat ["Could not parse UUID: ",decodeUtf8 s]
      Just uuid -> Right uuid
  fromPersistValue x = Left . pack $ "Not a PersistText type: " <> show x

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "UUID"

instance FromJSON UUID where
  parseJSON =
    withText "UUID"
             (\t ->
                case fromText t of
                  Nothing -> fail (unpack t)
                  Just uuid -> return uuid)

instance ToJSON UUID where
  toJSON = String . toText

instance PathPiece UUID where
  fromPathPiece = fromText
  toPathPiece = toText

instance ToHttpApiData UUID where
  toUrlPiece = toText

instance FromHttpApiData UUID where
  parseUrlPiece piece =
    case fromText piece of
      Nothing -> Left "Cannot parse UUID."
      Just x -> Right x

toStrictByteString :: UUID -> ByteString
toStrictByteString = encodeUtf8 . toText
