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
module Kashmir.Github.Types.Organization where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.Text                   hiding (drop)
import           Database.Persist.TH
import           GHC.Generics
import           Kashmir.Github.Types.Common

share [mkPersist sqlSettings {mpsGenerateLenses = True
                             ,mpsPrefixFields = False}
      ,mkMigrate "migrateGithubOrganization"]
      [persistLowerCase|
  Organization sql=github_organization
    githubOrganizationId Int
    login            Text sqltype=text
    url              URL sqltype=text
    reposUrl         URL sqltype=text
    eventsUrl        URL sqltype=text
    hooksUrl         URL sqltype=text
    issuesUrl        URL sqltype=text
    membersUrl       URL sqltype=text
    publicMembersUrl URL sqltype=text
    avatarUrl        URL sqltype=text
    description      Text Maybe sqltype=text
    Primary githubOrganizationId
    deriving Read Show Eq Generic
  |]


instance FromJSON Organization where
  parseJSON =
    genericParseJSON $
    defaultOptions {fieldLabelModifier =
                      \s ->
                        case s of
                          "_githubOrganizationId" -> "id"
                          _ -> drop 1 (snakeCase s)}
