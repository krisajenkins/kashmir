{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Kashmir.Github.Types.User where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.Text                   hiding (drop)
import           Data.Time
import           Database.Persist.TH
import           GHC.Generics
import           Kashmir.Email
import           Kashmir.Github.Types.Common

-- https://developer.github.com/v3/users/
share [mkPersist sqlSettings {mpsGenerateLenses = True
                             ,mpsPrefixFields = False}
      ,mkMigrate "migrateGithubUser"]
      [persistLowerCase|
  User sql=github_user
    githubUserId Int
    login              Text sqltype=text
    avatarUrl          URL sqltype=text
    gravatarId         URL Maybe sqltype=text
    url                URL sqltype=text
    htmlUrl            URL sqltype=text
    followersUrl       URL sqltype=text
    followingUrl       URL sqltype=text
    gistsUrl           URL sqltype=text
    starredUrl         URL sqltype=text
    subscriptionsUrl   URL sqltype=text
    organizationsUrl   URL sqltype=text
    reposUrl           URL sqltype=text
    eventsUrl          URL sqltype=text
    receivedEventsUrl  URL sqltype=text
    siteAdmin          Bool
    name               Text Maybe sqltype=text
    company            Text Maybe sqltype=text
    blog               Text Maybe sqltype=text
    location           Text Maybe sqltype=text
    email              Email
    hireable           Bool
    bio                Text Maybe sqltype=text
    publicRepos        Int
    publicGists        Int
    followers          Int
    following          Int
    createdAt          UTCTime
    updatedAt          UTCTime
    Primary githubUserId
    deriving Read Show Eq Generic
  |]

instance FromJSON User where
  parseJSON =
    genericParseJSON $
    defaultOptions {fieldLabelModifier =
                      \s ->
                        case s of
                          "_githubUserId" -> "id"
                          _ -> drop 1 (snakeCase s)}
