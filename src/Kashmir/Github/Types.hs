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
module Kashmir.Github.Types where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Text                   hiding (drop)
import           Data.Time
import           Database.Persist
import           Database.Persist.TH
import           GHC.Generics
import           Kashmir.Aeson
import           Kashmir.Email
import           Kashmir.Github.Types.Common

share [mkPersist sqlSettings {mpsGenerateLenses = True}
      ,mkMigrate "migrateGithub"]
      [persistLowerCase|
  GithubUser
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
    Primary login
    deriving Read Show Eq Generic

  GithubRepository
    ownerLogin       Text sqltype=text
    name             Text sqltype=text
    description      Text Maybe sqltype=text
    private          Bool
    fork             Bool
    url              URL sqltype=text
    htmlUrl          URL sqltype=text
    archiveUrl       URL sqltype=text
    assigneesUrl     URL sqltype=text
    blobsUrl         URL sqltype=text
    branchesUrl      URL sqltype=text
    cloneUrl         URL sqltype=text
    collaboratorsUrl URL sqltype=text
    commentsUrl      URL sqltype=text
    commitsUrl       URL sqltype=text
    compareUrl       URL sqltype=text
    contentsUrl      URL sqltype=text
    contributorsUrl  URL sqltype=text
    deploymentsUrl   URL sqltype=text
    downloadsUrl     URL sqltype=text
    eventsUrl        URL sqltype=text
    forksUrl         URL sqltype=text
    gitCommitsUrl    URL sqltype=text
    gitRefsUrl       URL sqltype=text
    gitTagsUrl       URL sqltype=text
    gitUrl           URL sqltype=text
    hooksUrl         URL sqltype=text
    issueCommentUrl  URL sqltype=text
    issueEventsUrl   URL sqltype=text
    issuesUrl        URL sqltype=text
    keysUrl          URL sqltype=text
    labelsUrl        URL sqltype=text
    languagesUrl     URL sqltype=text
    mergesUrl        URL sqltype=text
    milestonesUrl    URL sqltype=text
    mirrorUrl        URL Maybe sqltype=text
    notificationsUrl URL sqltype=text
    pullsUrl         URL sqltype=text
    releasesUrl      URL sqltype=text
    sshUrl           URL sqltype=text
    stargazersUrl    URL sqltype=text
    statusesUrl      URL sqltype=text
    subscribersUrl   URL sqltype=text
    subscriptionUrl  URL sqltype=text
    svnUrl           URL sqltype=text
    tagsUrl          URL sqltype=text
    teamsUrl         URL sqltype=text
    treesUrl         URL sqltype=text
    homepage         URL Maybe sqltype=text
    language         Text Maybe sqltype=text
    forksCount       Int
    stargazersCount  Int
    watchersCount    Int
    size             Int
    defaultBranch    Text sqltype=text
    openIssuesCount  Int
    hasIssues        Bool
    hasWiki          Bool
    hasPages         Bool
    hasDownloads     Bool
    pushedAt         UTCTime
    createdAt        UTCTime
    updatedAt        UTCTime
    Primary ownerLogin name
    deriving Show Eq Generic

  GithubOrganization
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
    Primary login
    deriving Read Show Eq Generic

  GithubRepositoryHook
    githubRepositoryHookId  Int
    githubRepositoryOwnerLogin Text sqltype=text
    githubRepositoryName Text sqltype=text
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
    Foreign GithubRepository fk_hook_parent githubRepositoryOwnerLogin githubRepositoryName
    deriving Show Eq Generic
  |]

instance FromJSON GithubUser where
  parseJSON =
    genericParseJSON $
    defaultOptions {fieldLabelModifier =
                      \s ->
                        case s of
                          "_githubUserGithubUserId" -> "id"
                          _ -> drop 13 (snakeCase s)}

instance FromJSON GithubOrganization where
  parseJSON =
    genericParseJSON $
    defaultOptions {fieldLabelModifier =
                      \s ->
                        case s of
                          "_githubOrganizationGithubOrganizationId" -> "id"
                          _ -> drop 21 (snakeCase s)}

instance FromJSON GithubRepository where
  parseJSON =
    withObject "repository" $
    \o ->
      do owner <- o .: "owner"
         _githubRepositoryOwnerLogin <- owner .: "login"
         _githubRepositoryName <- o .: "name"
         _githubRepositoryDescription <- o .: "description"
         _githubRepositoryPrivate <- o .: "private"
         _githubRepositoryFork <- o .: "fork"
         _githubRepositoryUrl <- o .: "url"
         _githubRepositoryHtmlUrl <- o .: "html_url"
         _githubRepositoryArchiveUrl <- o .: "archive_url"
         _githubRepositoryAssigneesUrl <- o .: "assignees_url"
         _githubRepositoryBlobsUrl <- o .: "blobs_url"
         _githubRepositoryBranchesUrl <- o .: "branches_url"
         _githubRepositoryCloneUrl <- o .: "clone_url"
         _githubRepositoryCollaboratorsUrl <- o .: "collaborators_url"
         _githubRepositoryCommentsUrl <- o .: "comments_url"
         _githubRepositoryCommitsUrl <- o .: "commits_url"
         _githubRepositoryCompareUrl <- o .: "compare_url"
         _githubRepositoryContentsUrl <- o .: "contents_url"
         _githubRepositoryContributorsUrl <- o .: "contributors_url"
         _githubRepositoryDeploymentsUrl <- o .: "deployments_url"
         _githubRepositoryDownloadsUrl <- o .: "downloads_url"
         _githubRepositoryEventsUrl <- o .: "events_url"
         _githubRepositoryForksUrl <- o .: "forks_url"
         _githubRepositoryGitCommitsUrl <- o .: "git_commits_url"
         _githubRepositoryGitRefsUrl <- o .: "git_refs_url"
         _githubRepositoryGitTagsUrl <- o .: "git_tags_url"
         _githubRepositoryGitUrl <- o .: "git_url"
         _githubRepositoryHooksUrl <- o .: "hooks_url"
         _githubRepositoryIssueCommentUrl <- o .: "issue_comment_url"
         _githubRepositoryIssueEventsUrl <- o .: "issue_events_url"
         _githubRepositoryIssuesUrl <- o .: "issues_url"
         _githubRepositoryKeysUrl <- o .: "keys_url"
         _githubRepositoryLabelsUrl <- o .: "labels_url"
         _githubRepositoryLanguagesUrl <- o .: "languages_url"
         _githubRepositoryMergesUrl <- o .: "merges_url"
         _githubRepositoryMilestonesUrl <- o .: "milestones_url"
         _githubRepositoryMirrorUrl <- o .: "mirror_url"
         _githubRepositoryNotificationsUrl <- o .: "notifications_url"
         _githubRepositoryPullsUrl <- o .: "pulls_url"
         _githubRepositoryReleasesUrl <- o .: "releases_url"
         _githubRepositorySshUrl <- o .: "ssh_url"
         _githubRepositoryStargazersUrl <- o .: "stargazers_url"
         _githubRepositoryStatusesUrl <- o .: "statuses_url"
         _githubRepositorySubscribersUrl <- o .: "subscribers_url"
         _githubRepositorySubscriptionUrl <- o .: "subscription_url"
         _githubRepositorySvnUrl <- o .: "svn_url"
         _githubRepositoryTagsUrl <- o .: "tags_url"
         _githubRepositoryTeamsUrl <- o .: "teams_url"
         _githubRepositoryTreesUrl <- o .: "trees_url"
         _githubRepositoryHomepage <- o .: "homepage"
         _githubRepositoryLanguage <- o .: "language"
         _githubRepositoryForksCount <- o .: "forks_count"
         _githubRepositoryStargazersCount <- o .: "stargazers_count"
         _githubRepositoryWatchersCount <- o .: "watchers_count"
         _githubRepositorySize <- o .: "size"
         _githubRepositoryDefaultBranch <- o .: "default_branch"
         _githubRepositoryOpenIssuesCount <- o .: "open_issues_count"
         _githubRepositoryHasIssues <- o .: "has_issues"
         _githubRepositoryHasWiki <- o .: "has_wiki"
         _githubRepositoryHasPages <- o .: "has_pages"
         _githubRepositoryHasDownloads <- o .: "has_downloads"
         _githubRepositoryPushedAt <- o .: "pushed_at"
         _githubRepositoryCreatedAt <- o .: "created_at"
         _githubRepositoryUpdatedAt <- o .: "updated_at"
         return GithubRepository {..}

data RawRepositoryHook =
  RawRepositoryHook {hookId    :: Int
                    ,url       :: URL
                    ,testUrl   :: URL
                    ,pingUrl   :: URL
                    ,name      :: Text
                    ,active    :: Bool
                    ,createdAt :: UTCTime
                    ,updatedAt :: UTCTime}
  deriving (Show,Eq,Generic)

instance FromJSON RawRepositoryHook where
  parseJSON =
    genericParseJSON $
    defaultOptions {fieldLabelModifier =
                      \s ->
                        case s of
                          "hookId" -> "id"
                          _ -> snakeCase s}

instance ToJSON GithubRepository

instance ToJSON GithubRepositoryHook

fromRaw :: Text -> Text -> RawRepositoryHook ->  GithubRepositoryHook
fromRaw ownerLogin repoName RawRepositoryHook{..} =
  let _githubRepositoryHookGithubRepositoryHookId = hookId
      _githubRepositoryHookGithubRepositoryOwnerLogin = ownerLogin
      _githubRepositoryHookGithubRepositoryName = repoName
      _githubRepositoryHookUrl = url
      _githubRepositoryHookTestUrl = testUrl
      _githubRepositoryHookPingUrl = pingUrl
      _githubRepositoryHookName = name
      _githubRepositoryHookActive = active
      _githubRepositoryHookCreatedAt = createdAt
      _githubRepositoryHookUpdatedAt = updatedAt
  in GithubRepositoryHook {..}

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

data Config =
  Config {_clientId     :: String
         ,_clientSecret :: String
         ,_authUrl      :: String
         ,_accessUrl    :: String}
  deriving (Eq,Show,Generic)

makeLenses ''Config
$(deriveJSON (dropPrefixJSONOptions "_")
             ''Config)
