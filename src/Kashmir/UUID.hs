{-# LANGUAGE InstanceSigs #-}
module Kashmir.UUID (toStrictByteString, module X) where

import           Data.Aeson
import           Data.ByteString      (ByteString)
import           Data.ByteString.Lazy (toStrict)
import           Data.Monoid
import           Data.Text
import           Data.Text.Encoding   (decodeUtf8, encodeUtf8)
import           Data.UUID            as X
import           Database.Persist
import           Database.Persist.Sql
import           Web.PathPieces

instance PersistField UUID where
  toPersistValue :: UUID -> PersistValue
  toPersistValue = PersistDbSpecific . encodeUtf8 . toText
  fromPersistValue :: PersistValue -> Either Text UUID
  fromPersistValue (PersistDbSpecific s) =
    case (fromText . decodeUtf8) s of
      Nothing ->
        Left $
        mconcat ["Could not parse UUID: ",decodeUtf8 s]
      Just uuid -> Right uuid
  fromPersistValue x = Left . pack $ "Not a PersistText type: " <> show x

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "UUID"

instance FromJSON UUID where
  parseJSON = withText "UUID" (\t -> case fromText t of
                                       Nothing -> fail (unpack t)
                                       Just uuid -> return uuid)

instance ToJSON UUID where
  toJSON = String . toText

instance PathPiece UUID where
  fromPathPiece = fromText
  toPathPiece = toText

toStrictByteString :: UUID -> ByteString
toStrictByteString = toStrict . toByteString
