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
module Kashmir.Github.Types.Repository where

import           Data.Aeson
import           Data.Text                   hiding (drop)
import           Data.Time
import           Database.Persist.TH
import           GHC.Generics
import           Kashmir.Github.Types.Common
import           Kashmir.Github.Types.User   (UserId)

share [mkPersist sqlSettings {mpsGenerateLenses = True
                             ,mpsPrefixFields = False}
      ,mkMigrate "migrateGithubRepository"]
      [persistLowerCase|
  Repository sql=github_repository
    githubRepositoryId  Int
    ownerId          Int
    name             Text sqltype=text
    fullName         Text sqltype=text
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
    Primary githubRepositoryId
    deriving Show Eq Generic
  |]

instance FromJSON Repository where
  parseJSON =
    withObject "repository" $
    \o ->
      do _githubRepositoryId <- o .: "id"
         owner <- o .: "owner"
         _ownerId <- owner .: "id"
         _name <- o .: "name"
         _fullName <- o .: "full_name"
         _description <- o .: "description"
         _private <- o .: "private"
         _fork <- o .: "fork"
         _url <- o .: "url"
         _htmlUrl <- o .: "html_url"
         _archiveUrl <- o .: "archive_url"
         _assigneesUrl <- o .: "assignees_url"
         _blobsUrl <- o .: "blobs_url"
         _branchesUrl <- o .: "branches_url"
         _cloneUrl <- o .: "clone_url"
         _collaboratorsUrl <- o .: "collaborators_url"
         _commentsUrl <- o .: "comments_url"
         _commitsUrl <- o .: "commits_url"
         _compareUrl <- o .: "compare_url"
         _contentsUrl <- o .: "contents_url"
         _contributorsUrl <- o .: "contributors_url"
         _deploymentsUrl <- o .: "deployments_url"
         _downloadsUrl <- o .: "downloads_url"
         _eventsUrl <- o .: "events_url"
         _forksUrl <- o .: "forks_url"
         _gitCommitsUrl <- o .: "git_commits_url"
         _gitRefsUrl <- o .: "git_refs_url"
         _gitTagsUrl <- o .: "git_tags_url"
         _gitUrl <- o .: "git_url"
         _hooksUrl <- o .: "hooks_url"
         _issueCommentUrl <- o .: "issue_comment_url"
         _issueEventsUrl <- o .: "issue_events_url"
         _issuesUrl <- o .: "issues_url"
         _keysUrl <- o .: "keys_url"
         _labelsUrl <- o .: "labels_url"
         _languagesUrl <- o .: "languages_url"
         _mergesUrl <- o .: "merges_url"
         _milestonesUrl <- o .: "milestones_url"
         _mirrorUrl <- o .: "mirror_url"
         _notificationsUrl <- o .: "notifications_url"
         _pullsUrl <- o .: "pulls_url"
         _releasesUrl <- o .: "releases_url"
         _sshUrl <- o .: "ssh_url"
         _stargazersUrl <- o .: "stargazers_url"
         _statusesUrl <- o .: "statuses_url"
         _subscribersUrl <- o .: "subscribers_url"
         _subscriptionUrl <- o .: "subscription_url"
         _svnUrl <- o .: "svn_url"
         _tagsUrl <- o .: "tags_url"
         _teamsUrl <- o .: "teams_url"
         _treesUrl <- o .: "trees_url"
         _homepage <- o .: "homepage"
         _language <- o .: "language"
         _forksCount <- o .: "forks_count"
         _stargazersCount <- o .: "stargazers_count"
         _watchersCount <- o .: "watchers_count"
         _size <- o .: "size"
         _defaultBranch <- o .: "default_branch"
         _openIssuesCount <- o .: "open_issues_count"
         _hasIssues <- o .: "has_issues"
         _hasWiki <- o .: "has_wiki"
         _hasPages <- o .: "has_pages"
         _hasDownloads <- o .: "has_downloads"
         _pushedAt <- o .: "pushed_at"
         _createdAt <- o .: "created_at"
         _updatedAt <- o .: "updated_at"
         return Repository {..}
