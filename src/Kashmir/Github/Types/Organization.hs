{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Kashmir.Github.Types.Organization where

import           Control.Lens
import           Data.Aeson.Casing
import           Data.Aeson.TH               (deriveJSON)
import           Data.Text
import           GHC.Generics
import           Kashmir.Github.Types.Common

data Organization =
  Organization {_login            :: Text
               ,_id               :: Integer
               ,_url              :: URL
               ,_reposUrl         :: URL
               ,_eventsUrl        :: URL
               ,_hooksUrl         :: URL
               ,_issuesUrl        :: URL
               ,_membersUrl       :: URL
               ,_publicMembersUrl :: URL
               ,_avatarUrl        :: URL
               ,_description      :: Maybe Text}
  deriving (Show,Eq,Generic)

makeLenses ''Organization
$(deriveJSON (aesonDrop 1 snakeCase)
             ''Organization)
