{-# LANGUAGE OverloadedStrings #-}
module Kashmir.GithubSpec where

import           Control.Error.Safe
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Data.Set                  as Set
import           Data.Text                 (pack)
import           Data.Yaml
import           Kashmir.Github
import qualified Kashmir.Github.Types.Hook as Hook
import           System.Environment
import           Test.Hspec
import           Test.QuickCheck.Instances ()

spec :: Spec
spec =
  do routesSpec
     userDetailsSpec
     userOrganizationSpec
     userRepositorySpec

routesSpec :: Spec
routesSpec =
  describe "URL Mapping" . it "Sitemap -> Text" $
  mapM_ (\(aUrl,siteMap) -> toUrl siteMap `shouldBe` aUrl)
        [("/user",UserDetails)
        ,("/user/orgs",UserOrganizations)
        ,("/user/repos",UserRepositories)
        ,("/repos/krisajenkins/kashmir"
         ,Repositories "krisajenkins" "kashmir" RepositoryDetails)
        ,("/repos/krisajenkins/kashmir/hooks"
         ,(Repositories "krisajenkins" "kashmir" RepositoryHooks))
        ,("/repos/krisajenkins/kashmir/hooks/12345"
         ,Repositories "krisajenkins"
                       "kashmir"
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

newHook :: Hook.Hook
newHook =
  Hook.Hook {Hook.name = Hook.Web
            ,Hook.events = Set.singleton Hook.Push
            ,Hook.active = True
            ,Hook.config =
               Hook.HookConfig {Hook.url = "https://www.jenkster.com/kashmir"
                               ,Hook.contentType = Hook.Json
                               ,Hook.secret = Nothing
                               ,Hook.insecureSsl = False}}

repositoryHooksSpec :: Spec
repositoryHooksSpec =
  let ownerId = "krisajenkins"
      repoId = "kashmir"
  in describe "Repository hook fetching" $
     do it "Fetches the current repository hooks." . withToken $
          void (getRepositoryHooks ownerId repoId)
        it "Creates and deletes a hook" . withToken $
          void (do Right createdHook <-
                     createRepositoryHook ownerId repoId newHook
                   let createdHookId =
                         view githubRepositoryHookGithubRepositoryHookId createdHook
                   deleteRepositoryHook ownerId repoId createdHookId)

loadConfig :: IO (Either ParseException AccessToken)
loadConfig = decodeFileEither "/Users/kris/Work/OpenSource/Kashmir/kashmir.yaml"

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
