{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Kashmir.Github.Types.Repository where

import           Control.Lens
import           Data.Aeson.Casing
import           Data.Aeson.TH               (deriveJSON)
import           Data.Text
import           Data.Time
import           GHC.Generics
import           Kashmir.Github.Types.Common

data Permissions =
  Permissions {_admin :: Bool
              ,_push  :: Bool
              ,_pull  :: Bool}
  deriving (Show,Eq,Generic)

makeLenses ''Permissions
$(deriveJSON (aesonDrop 1 snakeCase)
             ''Permissions)

data Repository =
  Repository {_id               :: Integer
             ,_name             :: Text
             ,_fullName         :: Text
             ,_description      :: Maybe Text
             ,_private          :: Bool
             ,_fork             :: Bool
             ,_url              :: URL
             ,_htmlUrl          :: URL
             ,_archiveUrl       :: URL
             ,_assigneesUrl     :: URL
             ,_blobsUrl         :: URL
             ,_branchesUrl      :: URL
             ,_cloneUrl         :: URL
             ,_collaboratorsUrl :: URL
             ,_commentsUrl      :: URL
             ,_commitsUrl       :: URL
             ,_compareUrl       :: URL
             ,_contentsUrl      :: URL
             ,_contributorsUrl  :: URL
             ,_deploymentsUrl   :: URL
             ,_downloadsUrl     :: URL
             ,_eventsUrl        :: URL
             ,_forksUrl         :: URL
             ,_gitCommitsUrl    :: URL
             ,_gitRefsUrl       :: URL
             ,_gitTagsUrl       :: URL
             ,_gitUrl           :: URL
             ,_hooksUrl         :: URL
             ,_issueCommentUrl  :: URL
             ,_issueEventsUrl   :: URL
             ,_issuesUrl        :: URL
             ,_keysUrl          :: URL
             ,_labelsUrl        :: URL
             ,_languagesUrl     :: URL
             ,_mergesUrl        :: URL
             ,_milestonesUrl    :: URL
             ,_mirrorUrl        :: Maybe URL
             ,_notificationsUrl :: URL
             ,_pullsUrl         :: URL
             ,_releasesUrl      :: URL
             ,_sshUrl           :: URL
             ,_stargazersUrl    :: URL
             ,_statusesUrl      :: URL
             ,_subscribersUrl   :: URL
             ,_subscriptionUrl  :: URL
             ,_svnUrl           :: URL
             ,_tagsUrl          :: URL
             ,_teamsUrl         :: URL
             ,_treesUrl         :: URL
             ,_homepage         :: Maybe URL
             ,_language         :: Maybe Text
             ,_forksCount       :: Int
             ,_stargazersCount  :: Int
             ,_watchersCount    :: Int
             ,_size             :: Int
             ,_defaultBranch    :: Text
             ,_openIssuesCount  :: Int
             ,_hasIssues        :: Bool
             ,_hasWiki          :: Bool
             ,_hasPages         :: Bool
             ,_hasDownloads     :: Bool
             ,_pushedAt         :: UTCTime
             ,_createdAt        :: UTCTime
             ,_updatedAt        :: UTCTime
             ,_permissions      :: Permissions}
  deriving (Show,Eq,Generic)

makeLenses ''Repository
$(deriveJSON (aesonDrop 1 snakeCase)
             ''Repository)
