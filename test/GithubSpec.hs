{-# LANGUAGE OverloadedStrings #-}
module GithubSpec where

import           Control.Error.Safe
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Data.Set                            as Set
import           Data.Text                           (pack)
import           Data.Yaml
import           Kashmir.Github
import           Kashmir.Github.Types.Hook           as Hook
import qualified Kashmir.Github.Types.RepositoryHook as RH
import           System.Environment
import           Test.Hspec
import           Test.QuickCheck.Instances           ()

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
  mapM_ (\(aUrl,siteMap) -> toUrl siteMap `shouldBe` aUrl)
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
  describe "User fetching" . it "Fetches the current user." . withToken $
  void getUserDetails

userOrganizationSpec :: Spec
userOrganizationSpec =
  describe "Organization fetching" . it "Fetches the current organizations." . withToken $
  void getUserOrganizations

userRepositorySpec :: Spec
userRepositorySpec =
  describe "Repository fetching" .
  it "Fetches the current repositories." . withToken $
  do repos <- getUserRepositories
     liftIO $ length repos `shouldSatisfy` (> 50)

repositoryHooksSpec :: Spec
repositoryHooksSpec =
  let username = "krisajenkins"
      repoName = "autoheadline"
  in describe "Repository hook fetching" $
     do it "Fetches the current repository hooks." . withToken $
          void (getRepositoryHooks username repoName)
        it "Creates and deletes a hook" . withToken $
          (do let newHook =
                    Hook {name = Web
                         ,events = Set.singleton Push
                         ,active = False
                         ,config =
                            HookConfig {url =
                                          "https://www.jenkster.com/kashmir"
                                       ,contentType = Json
                                       ,secret = Nothing
                                       ,insecureSsl = False}}
              createdHook <- createRepositoryHook username repoName newHook
              let createdHookId = view RH.githubRepositoryHookId createdHook
              deleteRepositoryHook username repoName createdHookId)

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
         do aConfig <- loadConfig
            case aConfig of
              Left e -> fail (show e)
              Right aToken -> return aToken

withToken :: ReaderT AccessToken IO a -> IO a
withToken block = loadToken >>= runReaderT block
