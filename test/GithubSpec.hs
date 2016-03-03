{-# LANGUAGE OverloadedStrings #-}
module GithubSpec where

import           Control.Error.Safe
import           Control.Monad
import           Data.Text                 (pack)
import           Data.Yaml
import           Kashmir.Github
import           System.Environment
import           Test.Hspec
import           Test.QuickCheck.Instances ()

spec :: Spec
spec =
  do userDetailsSpec
     userOrganizationSpec
     userRepositorySpec

userDetailsSpec :: Spec
userDetailsSpec =
  describe "User fetching" $
  it "Fetches the current user." $ void (loadToken >>= getUserDetails)

userOrganizationSpec :: Spec
userOrganizationSpec =
  describe "Organization fetching" $
  it "Fetches the current organizations." $
  void (loadToken >>= getUserOrganizations)

userRepositorySpec :: Spec
userRepositorySpec =
  describe "Repository fetching" $
  it "Fetches the current repositories." $
  do repos <- loadToken >>= getUserRepositories
     length repos `shouldSatisfy` (> 50)

loadConfig :: IO (Either ParseException AccessToken)
loadConfig = decodeFileEither "kashmir.yaml"

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
