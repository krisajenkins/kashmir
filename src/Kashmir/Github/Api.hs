{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Kashmir.Github.Api
       (getUserDetails, getUserOrganizations, getUserRepositories,requestAccess)
       where

import           Control.Category                  ((.))
import           Control.Lens
import           Data.Aeson
import           Data.ByteString                   hiding (pack, putStrLn,
                                                    unpack)
import           Data.Monoid
import           Data.Text
import           Data.Text.Encoding
import           Kashmir.Github.Types
import           Kashmir.Github.Types.Common
import           Kashmir.Github.Types.Organization
import           Kashmir.Github.Types.Repository
import           Kashmir.Github.Types.User
import           Kashmir.Web                       (mimeTypeJson)
import           Network.Wreq
import           Prelude                           hiding (id, (.))
import           Text.Boomerang.HStack
import           Text.Boomerang.TH
import           Web.Routes                        hiding (URL)
import           Web.Routes.Boomerang              hiding ((.~))

data Sitemap
  = UserDetails
  | UserOrganizations
  | UserRepositories
  deriving (Eq,Ord,Read,Show)

makeBoomerangs ''Sitemap

sitemap :: Router () (Sitemap :- ())
sitemap = mconcat ["user" . users]
  where users =
          mconcat [rUserDetails
                  ,rUserOrganizations </> "orgs"
                  ,rUserRepositories </> "repos"]

handle ::  Sitemap -> RouteT Sitemap IO Text
handle aUrl =
  case aUrl of
    UserDetails -> return "index!"

site :: Site Sitemap (IO Text)
site = boomerangSiteRouteT handle sitemap

showSitemap :: Sitemap -> Text
showSitemap aUrl = uncurry encodePathInfo $ formatPathSegments site aUrl

server :: Text
server = "https://api.github.com"

makeGithubUrl :: Sitemap -> Text
makeGithubUrl uri = server <> (showSitemap uri)

unfoldPages
  :: Monad m
  => (b -> m (Maybe ([a],b))) -> b -> m [a]
unfoldPages f b =
  do r <- f b
     case r of
       Just (page,b') ->
         do pages <- unfoldPages f b'
            return $ page <> pages
       Nothing -> return []

getRaw :: FromJSON a => AccessToken -> URL -> IO (Response a)
getRaw (AccessToken t) aUrl =
  getWith (defaults & param "access_token" .~ [t])
          (unpack aUrl) >>=
  asJSON

githubGetPage
  :: FromJSON a
  => AccessToken -> Maybe URL -> IO (Maybe ([a],Maybe URL))
githubGetPage aToken maybeUrl =
  case maybeUrl of
    Nothing -> return Nothing
    Just uri ->
      do r <- getRaw aToken uri
         return $
           Just (r ^. responseBody
                ,decodeUtf8 <$> (r ^? responseLink "rel" "next" . linkURL))

githubGet :: FromJSON a
          => Sitemap -> AccessToken -> IO a
githubGet uri aToken = view responseBody <$> getRaw aToken (makeGithubUrl uri)

githubGetPages :: FromJSON a
               => Sitemap -> AccessToken -> IO [a]
githubGetPages uri t =
  unfoldPages (githubGetPage t)
              (Just (makeGithubUrl uri))

getUserDetails :: AccessToken -> IO User
getUserDetails = githubGet UserDetails

getUserOrganizations :: AccessToken -> IO [Organization]
getUserOrganizations = githubGetPages UserOrganizations

getUserRepositories :: AccessToken -> IO [Repository]
getUserRepositories = githubGetPages UserRepositories

-- TODO This doesn't handle a response of:
--  responseBody = "{\"error\":\"bad_verification_code\",\"error_description\":\"The code passed is incorrect or expired.\",\"error_uri\":\"https://developer.github.com/v3/oauth/#bad-verification-code\"}"
requestAccess :: Config -> ByteString -> IO AccessTokenResponse
requestAccess config code =
  do r :: Response AccessTokenResponse <-
       asJSON =<<
       postWith (defaults & header "Accept" .~ [mimeTypeJson])
                (view accessUrl config)
                ["code" := code
                ,"client_id" := view clientId config
                ,"client_secret" := view clientSecret config]
     return (view responseBody r)
