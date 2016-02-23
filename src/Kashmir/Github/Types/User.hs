{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Kashmir.Github.Types.User where

import           Control.Lens
import           Data.Aeson.Casing
import           Data.Aeson.TH               (deriveJSON)
import           Data.Text
import           Data.Time
import           GHC.Generics
import           Kashmir.Email
import           Kashmir.Github.Types.Common

-- https://developer.github.com/v3/users/
data User =
  User {_login             :: Text
       ,_id                :: Int
       ,_avatarUrl         :: URL
       ,_gravatarId        :: Maybe URL
       ,_url               :: URL
       ,_htmlUrl           :: URL
       ,_followersUrl      :: URL
       ,_followingUrl      :: URL
       ,_gistsUrl          :: URL
       ,_starredUrl        :: URL
       ,_subscriptionsUrl  :: URL
       ,_organizationsUrl  :: URL
       ,_reposUrl          :: URL
       ,_eventsUrl         :: URL
       ,_receivedEventsUrl :: URL
       ,_siteAdmin         :: Bool
       ,_name              :: Maybe Text
       ,_company           :: Maybe Text
       ,_blog              :: Maybe Text
       ,_location          :: Maybe Text
       ,_email             :: Email
       ,_hireable          :: Bool
       ,_bio               :: Maybe Text
       ,_publicRepos       :: Int
       ,_publicGists       :: Int
       ,_followers         :: Int
       ,_following         :: Int
       ,_createdAt         :: UTCTime
       ,_updatedAt         :: UTCTime}
  deriving (Show,Eq,Generic)

makeLenses ''User
$(deriveJSON (aesonDrop 1 snakeCase)
             ''User)
