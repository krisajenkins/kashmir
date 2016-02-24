{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Kashmir.Github.Api
       (getUser, getOrganizations, getRepositories, requestAccess) where

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
import           Network.Wreq

server :: Text
server = "https://api.github.com"

makeGithubUrl :: Text -> Text
makeGithubUrl uri = server <> uri

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
          => URL -> AccessToken -> IO a
githubGet uri aToken = view responseBody <$> getRaw aToken (makeGithubUrl uri)

githubGetPages :: FromJSON a
               => URL -> AccessToken -> IO [a]
githubGetPages uri t =
  unfoldPages (githubGetPage t)
              (Just (makeGithubUrl uri))

getUser :: AccessToken -> IO User
getUser = githubGet "/user"

getOrganizations :: AccessToken -> IO [Organization]
getOrganizations = githubGetPages "/user/orgs"

getRepositories :: AccessToken -> IO [Repository]
getRepositories = githubGetPages "/user/repos"


-- TODO This doesn't handle a response of:
--  responseBody = "{\"error\":\"bad_verification_code\",\"error_description\":\"The code passed is incorrect or expired.\",\"error_uri\":\"https://developer.github.com/v3/oauth/#bad-verification-code\"}"
requestAccess :: Config -> ByteString -> IO AccessTokenResponse
requestAccess config code =
  do r :: Response AccessTokenResponse <-
       asJSON =<<
       postWith (defaults & header "Accept" .~ ["application/json"])
                (view accessUrl config)
                ["code" := code
                ,"client_id" := view clientId config
                ,"client_secret" := view clientSecret config]
     return (view responseBody r)
