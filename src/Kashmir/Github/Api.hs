{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
module Kashmir.Github.Api
       (getUserDetails, getUserOrganizations, getUserRepositories,getRepositoryHooks
       ,requestAccess,createRepositoryHook,deleteRepositoryHook,toUrl,Sitemap(..),RepositorySitemap(..),GithubError(..),GithubErrorMessage(..))
       where

import           Control.Category            ((.))
import           Control.Exception           (try)
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Bifunctor
import           Data.ByteString             hiding (pack, putStrLn, unpack)
import           Data.ByteString.Lazy        (fromStrict)
import qualified Data.CaseInsensitive        as CI
import           Data.Map                    (Map)
import           Data.Maybe
import           Data.Monoid
import           Data.Text
import           Data.Text.Encoding
import           Kashmir.Github.Types
import           Kashmir.Github.Types.Common
import           Kashmir.Github.Types.Hook   (Hook)
import           Kashmir.Unfold
import           Kashmir.Web                 (mimeTypeJson)
import           Network.HTTP.Client         (HttpException (..))
import           Network.HTTP.Types.Status   (statusCode)
import           Network.Wreq                hiding (statusCode)
import           Prelude                     hiding ((.))
import           Text.Boomerang.HStack
import           Text.Boomerang.TH
import           Text.Printf
import           Web.Routes                  hiding (URL)
import           Web.Routes.Boomerang        hiding ((.~))

data RepositorySitemap
  = RepositoryDetails
  | RepositoryHooks
  | RepositoryHook Int
  deriving (Eq,Ord,Read,Show)

data Sitemap
  = UserDetails
  | UserOrganizations
  | UserRepositories
  | Repositories Text Text RepositorySitemap
  deriving (Eq,Ord,Read,Show)

makeBoomerangs ''RepositorySitemap
makeBoomerangs ''Sitemap

sitemap :: Router () (Sitemap :- ())
sitemap =
  mconcat ["user" . users
          ,"repos" . rRepositories </> anyText </> anyText . repos]
  where users =
          mconcat [rUserDetails
                  ,rUserOrganizations </> "orgs"
                  ,rUserRepositories </> "repos"]
        repos =
          mconcat [rRepositoryDetails
                  ,rRepositoryHooks </> "hooks"
                  ,rRepositoryHook </> "hooks" </> int]

toUrl :: Sitemap -> Text
toUrl s =
  fromMaybe (error (printf "Cannot convert to URL: %s" (show s)))
            (toPathInfo <$> unparseTexts sitemap s)


server :: Text
server = "https://api.github.com"

makeGithubUrl :: Sitemap -> Text
makeGithubUrl uri = server <> toUrl uri

getRaw
  :: FromJSON a
  => URL -> ReaderT AccessToken IO (Response a)
getRaw aUrl =
  do (AccessToken t) <- ask
     liftIO $
       do raw <-
            getWith (defaults & param "access_token" .~ [t])
                    (unpack aUrl)
          asJSON raw

postRaw
  :: (ToJSON a,FromJSON b)
  => URL -> a -> ReaderT AccessToken IO b
postRaw aUrl payload =
  do (AccessToken t) <- ask
     liftIO $
       do response <-
            postWith (defaults & param "access_token" .~ [t])
                     (unpack aUrl)
                     (toJSON payload)
          view responseBody <$> asJSON response

deleteRaw :: Text -> ReaderT AccessToken IO ()
deleteRaw aUrl =
  do (AccessToken t) <- ask
     void . liftIO $
       deleteWith (defaults & param "access_token" .~ [t])
                  (unpack aUrl)

postGithub
  :: (ToJSON a,FromJSON b)
  => Sitemap -> a -> ReaderT AccessToken IO b
postGithub uri = postRaw (makeGithubUrl uri)

deleteGithub :: Sitemap -> ReaderT AccessToken IO ()
deleteGithub uri = deleteRaw (makeGithubUrl uri)

githubGetPage
  :: FromJSON a
  => Maybe URL -> ReaderT AccessToken IO (Maybe ([a],Maybe URL))
githubGetPage maybeUrl =
  case maybeUrl of
    Nothing -> return Nothing
    Just uri ->
      do r <- getRaw uri
         return $
           Just (r ^. responseBody
                ,decodeUtf8 <$> (r ^? responseLink "rel" "next" . linkURL))

githubGet :: FromJSON a
          => Sitemap -> ReaderT AccessToken IO a
githubGet uri = view responseBody <$> getRaw (makeGithubUrl uri)

githubGetPages
  :: FromJSON a
  => Sitemap -> ReaderT AccessToken IO [a]
githubGetPages uri = unfoldrM githubGetPage (Just (makeGithubUrl uri))

getUserDetails :: ReaderT AccessToken IO GithubUser
getUserDetails = githubGet UserDetails

getUserOrganizations :: ReaderT AccessToken IO [GithubOrganization]
getUserOrganizations = githubGetPages UserOrganizations

getUserRepositories :: ReaderT AccessToken IO [GithubRepository]
getUserRepositories = githubGetPages UserRepositories

data GithubErrorMessage =
  GithubErrorMessage {message :: Text
                      ,errors :: [Map Text Text]}
  deriving (Show,Generic,FromJSON)

data GithubError
  = GithubError GithubErrorMessage
  | UnhandledGithubError HttpException
  deriving (Show,Generic)

handleGithubError :: HttpException -> GithubError
handleGithubError err@(StatusCodeException (statusCode -> 422) headerList _) =
  let rawMessage :: Maybe GithubErrorMessage =
        lookup (CI.mk "X-Response-Body-Start") headerList >>=
        decode . fromStrict
  in case rawMessage of
       Nothing -> UnhandledGithubError err
       Just message -> GithubError message

handleGithubError err = UnhandledGithubError err

tryGithub :: IO a -> IO (Either GithubError a)
tryGithub block = first handleGithubError <$> try block

getRepositoryHooks :: Text -> Text -> ReaderT AccessToken IO (Either GithubError [GithubRepositoryHook])
getRepositoryHooks ownerLogin repoName =
  mapReaderT tryGithub $
  do rawHooks <- githubGetPages (Repositories ownerLogin repoName RepositoryHooks)
     return (fromRaw ownerLogin repoName <$> rawHooks)

-- TODO This doesn't handle a response of:
--  responseBody = "{\"error\":\"bad_verification_code\",\"error_description\":\"The code passed is incorrect or expired.\",\"error_uri\":\"https://developer.github.com/v3/oauth/#bad-verification-code\"}"
requestAccess :: Config -> ByteString -> IO AccessTokenResponse
requestAccess config code =
  do response <-
       postWith (defaults & header "Accept" .~ [mimeTypeJson])
                (view accessUrl config)
                ["code" := code
                ,"client_id" := view clientId config
                ,"client_secret" := view clientSecret config]
     view responseBody <$> asJSON response

createRepositoryHook
  :: Text -> Text -> Hook -> ReaderT AccessToken IO (Either GithubError GithubRepositoryHook)
createRepositoryHook ownerLogin repoName hook =
  mapReaderT tryGithub $
  do (rawHook :: RawRepositoryHook) <-
       postGithub (Repositories ownerLogin repoName RepositoryHooks)
                  hook
     return (fromRaw ownerLogin repoName rawHook)

deleteRepositoryHook
  :: Text -> Text -> Int -> ReaderT AccessToken IO (Either GithubError ())
deleteRepositoryHook ownerLogin repoName hook =
  mapReaderT tryGithub $
  deleteGithub (Repositories ownerLogin repoName (RepositoryHook hook))
