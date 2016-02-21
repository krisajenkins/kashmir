{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Kashmir.Github.Types.User where

import           Control.Lens
import           Data.Aeson.Casing
import           Data.Aeson.TH               (deriveJSON)
import           Data.Text
import           GHC.Generics
import           Kashmir.Github.Types.Common

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
