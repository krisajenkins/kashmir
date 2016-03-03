{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Kashmir.Github.Types.RepositoryHook where

import           Data.Aeson
import           Data.Text                   hiding (drop)
import           Data.Time
import           Database.Persist.TH
import           GHC.Generics
import           Kashmir.Github.Types.Common

share [mkPersist sqlSettings {mpsGenerateLenses = True
                             ,mpsPrefixFields = False}
      ,mkMigrate "migrateGithubRepository"]
      [persistLowerCase|
  RepositoryHook sql=github_repository_hook
    githubRepositoryHookId  Int
    url          URL sqltype=text
    testUrl      URL sqltype=text
    pingUrl      URL sqltype=text
    name         Text sqltype=text
    -- "events": [
    --   "push",
    --   "pull_request"
    -- ],
    active       Bool
    -- "config": {
    --   "url": "http://example.com/webhook",
    --   "content_type": "json"
    -- },
    updatedAt    UTCTime
    createdAt    UTCTime
    Primary githubRepositoryHookId
    deriving Show Eq Generic
  |]

instance FromJSON RepositoryHook where
  parseJSON =
    withObject "repositoryHook" $
    \o ->
      do _githubRepositoryHookId <- o .: "id"
         _url <- o .: "url"
         _testUrl <- o .: "test_url"
         _pingUrl <- o .: "ping_url"
         _name <- o .: "name"
         _active <- o .: "active"
         _createdAt <- o .: "created_at"
         _updatedAt <- o .: "updated_at"
         return RepositoryHook {..}

instance ToJSON RepositoryHook
