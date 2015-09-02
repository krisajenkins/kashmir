{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Kashmir.Github.Types where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH     (deriveJSON)
import           Data.Text
import           Database.Persist
import           GHC.Generics
import           Kashmir.Aeson


newtype AccessToken = AccessToken { _token :: Text}
  deriving (Show,Read,Eq,Generic)

makeLenses ''AccessToken

instance PersistField AccessToken where
  toPersistValue = PersistText . view token
  fromPersistValue (PersistText t) = Right $ AccessToken t
  fromPersistValue _ = Left "Not a text persist type"

instance FromJSON AccessToken where
  parseJSON = withText "AccessToken" $ return . AccessToken

instance ToJSON AccessToken where
  toJSON (AccessToken t) = toJSON t

data AccessTokenResponse =
  AccessTokenResponse {_accessToken :: AccessToken
                      ,_scope       :: Text
                      ,_tokenType   :: Text}
  deriving (Show,Eq,Generic)

makeLenses ''AccessTokenResponse
$(deriveJSON (aesonDrop 1 snakeCase)
             ''AccessTokenResponse)

type URL = Text

-- https://developer.github.com/v3/users/
data User =
  User {_id                :: Int
       ,_login             :: Text
       ,_name              :: Text
       ,_email             :: Maybe Text
       ,_avatarUrl         :: URL
       ,_htmlUrl           :: URL
       ,_reposUrl          :: URL
       ,_receivedEventsUrl :: URL
       ,_company           :: Maybe Text
       ,_blog              :: Maybe Text
       ,_location          :: Text
       ,_hireable          :: Bool
       ,_bio               :: Maybe Text
       ,_followers         :: Integer
       ,_following         :: Integer}
  deriving (Show,Eq,Generic)

makeLenses ''User
$(deriveJSON (aesonDrop 1 snakeCase)
             ''User)

data Config =
  Config {_clientId     :: String
         ,_clientSecret :: String
         ,_authUrl      :: String
         ,_accessUrl    :: String}
  deriving (Eq,Show,Generic)

makeLenses ''Config
$(deriveJSON (dropPrefixJSONOptions "_")
             ''Config)
