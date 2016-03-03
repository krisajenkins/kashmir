{-# LANGUAGE OverloadedStrings #-}
module GithubSpec where

import           Control.Error.Safe
import           Control.Monad
import           Control.Monad.Reader
import           Data.Text                 (pack)
import           Data.Yaml
import           Kashmir.Github
import           System.Environment
import           Test.Hspec
import           Test.QuickCheck.Instances ()

spec :: Spec
spec =
  do routesSpec
     userDetailsSpec
     userOrganizationSpec
     userRepositorySpec
     repositoryHooksSpec

routesSpec :: Spec
routesSpec =
  describe "URL Mapping" . it "Sitemap -> Text" $
  mapM_ (\(url,siteMap) -> toUrl siteMap `shouldBe` url)
        [("/user",UserDetails)
        ,("/user/orgs",UserOrganizations)
        ,("/user/repos",UserRepositories)
        ,("/repos/kris/project",Repositories "kris" "project" RepositoryDetails)
        ,("/repos/kris/project/hooks"
         ,(Repositories "kris" "project" RepositoryHooks))
        ,("/repos/kris/project/hooks/12345"
         ,Repositories "kris"
                       "project"
                       (RepositoryHook 12345))]

userDetailsSpec :: Spec
userDetailsSpec =
  describe "User fetching" . it "Fetches the current user." $
  void (withToken getUserDetails)

userOrganizationSpec :: Spec
userOrganizationSpec =
  describe "Organization fetching" . it "Fetches the current organizations." $
  void (withToken getUserOrganizations)

userRepositorySpec :: Spec
userRepositorySpec =
  describe "Repository fetching" . it "Fetches the current repositories." $
  do repos <- withToken getUserRepositories
     length repos `shouldSatisfy` (> 50)

repositoryHooksSpec :: Spec
repositoryHooksSpec =
  describe "Repository hook fetching" $
  it "Fetches the current repository hooks." $
  void (withToken (getRepositoryHooks "krisajenkins" "yesql"))

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

withToken :: ReaderT AccessToken IO a -> IO a
withToken block = loadToken >>= runReaderT block
