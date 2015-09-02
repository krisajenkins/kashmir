{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Kashmir.Snap.Snaplet.Authentication.Config where

import           Control.Lens
import           Data.Aeson.TH               (deriveJSON)
import           Data.Text
import           GHC.Generics
import           Kashmir.Aeson
import           Kashmir.Database.Postgresql (DatabaseConfig)
import qualified Kashmir.Github              as Github

-- TODO The secret should probably be of type Web.JWT.Secret.
-- That should be as simple as writing a custom fromJSON for Secrets.
data WebserverConfig =
  WebserverConfig {_rootDirectory :: Text
                  ,_hostname      :: Text
                  ,_jwtSecretKey  :: Text}
  deriving (Eq,Show,Generic)

makeLenses ''WebserverConfig
$(deriveJSON (dropPrefixJSONOptions "_") ''WebserverConfig)

data TwitterConfig =
  TwitterConfig {_consumerKey          :: String
                ,_consumerSecret       :: String
                ,_requestTokenUrl      :: String
                ,_authorizeUrl         :: String
                ,_accessTokenUrl       :: String
                ,_appAuthenticationUrl :: String
                ,_callbackUrl          :: String}
  deriving (Eq,Show,Generic)

makeLenses ''TwitterConfig
$(deriveJSON (dropPrefixJSONOptions "_") ''TwitterConfig)

data Config =
  Config {_database  :: DatabaseConfig
         ,_webserver :: WebserverConfig
         ,_twitter   :: TwitterConfig
         ,_github    :: Github.Config}
  deriving (Eq,Show,Generic)

makeLenses ''Config
$(deriveJSON (dropPrefixJSONOptions "_") ''Config)
