{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Kashmir.Github.Api
       (getUserDetails, getUserOrganizations, getUserRepositories,getRepositoryHooks
       ,requestAccess,createRepositoryHook,deleteRepositoryHook,toUrl,Sitemap(..),RepositorySitemap(..))
       where

import           Control.Category                    ((.))
import           Control.Exception                   (try)
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson
import           Data.ByteString                     hiding (pack, putStrLn,
                                                      unpack)
import           Data.Maybe
import           Data.Monoid
import           Data.Text
import           Data.Text.Encoding
import           Kashmir.Github.Types
import           Kashmir.Github.Types.Common
import           Kashmir.Github.Types.Hook           (Hook)
import           Kashmir.Github.Types.Organization
import           Kashmir.Github.Types.Repository
import qualified Kashmir.Github.Types.RepositoryHook as RH
import           Kashmir.Github.Types.User
import           Kashmir.Unfold
import           Kashmir.Web                         (mimeTypeJson)
import           Network.Wreq
import           Prelude                             hiding ((.))
import           Text.Boomerang.HStack
import           Text.Boomerang.TH
import           Text.Printf
import           Web.Routes                          hiding (URL)
import           Web.Routes.Boomerang                hiding ((.~))

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

getUserDetails :: ReaderT AccessToken IO User
getUserDetails = githubGet UserDetails

getUserOrganizations :: ReaderT AccessToken IO [Organization]
getUserOrganizations = githubGetPages UserOrganizations

getUserRepositories :: ReaderT AccessToken IO [Repository]
getUserRepositories = githubGetPages UserRepositories

getRepositoryHooks :: Text -> Text -> ReaderT AccessToken IO (Either JSONError [RH.RepositoryHook])
getRepositoryHooks user repo =
  mapReaderT try $ githubGetPages (Repositories user repo RepositoryHooks)

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
  :: Text -> Text -> Hook -> ReaderT AccessToken IO RH.RepositoryHook
createRepositoryHook user repo =
  postGithub (Repositories user repo RepositoryHooks)

deleteRepositoryHook
  :: Text -> Text -> Int -> ReaderT AccessToken IO ()
deleteRepositoryHook user repo hookId =
  deleteGithub (Repositories user repo (RepositoryHook hookId))
