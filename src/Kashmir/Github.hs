module Kashmir.Github (module X, migrateGithub) where

import           Database.Persist.Sql
import           Kashmir.Github.Api                as X
import           Kashmir.Github.Types              as X
import           Kashmir.Github.Types.Organization as Organization
import           Kashmir.Github.Types.Repository   as Repository
import           Kashmir.Github.Types.User         as User

migrateGithub :: Migration
migrateGithub =
  Organization.migrateGithubOrganization >> Repository.migrateGithubRepository >>
  User.migrateGithubUser
