{-# LANGUAGE OverloadedStrings #-}
module GithubSpec where

import           Control.Error.Safe
import           Control.Lens
import           Data.Monoid
import           Data.Text                         (pack)
import           Data.Yaml
import           Kashmir.Github
import           Kashmir.Github.Types.Organization as GOrg
import           Kashmir.Github.Types.Repository   as GRepo
import           System.Environment
import           Test.Hspec
import           Test.QuickCheck.Instances         ()

spec :: Spec
spec =
  do userSpec
     organizationSpec
     repositorySpec

userSpec :: Spec
userSpec =
  describe "User fetching" $
  do it "Fetches the current user." $
       do user <- loadToken >>= getUser
          print user

organizationSpec :: Spec
organizationSpec =
  describe "Organization fetching" $
  do it "Fetches the current organizations." $
       do orgs <- loadToken >>= getOrganizations
          print (view GOrg.login <$> orgs)

repositorySpec :: Spec
repositorySpec =
  describe "Repository fetching" $
  do it "Fetches the current repositories." $
       do repos <- loadToken >>= getRepositories
          print (view GRepo.name <$> repos)
          length repos `shouldSatisfy` (> 50)

loadConfig :: IO (Either ParseException AccessToken)
loadConfig =
  do let configFile = "kashmir.yaml"
     putStrLn $ "Reading config: " <> configFile
     decodeFileEither configFile

loadToken :: IO AccessToken
loadToken =
  do envToken <-
       justErr ("Env var not set" :: String) <$>
       lookupEnv "GITHUB_ACCESS_TOKEN"
     case envToken of
       Right s -> return . AccessToken $ pack s
       Left _ ->
         do config <- loadConfig
            case config of
              Left e -> fail (show e)
              Right aToken -> return aToken
