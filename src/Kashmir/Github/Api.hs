{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Kashmir.Github.Api
       (getUser, getOrganizations, requestAccess) where

import           Control.Lens
import           Data.Aeson
import           Data.ByteString                   hiding (putStrLn, unpack)
import           Data.Monoid
import           Data.Text
import           Kashmir.Github.Types
import           Kashmir.Github.Types.Common
import           Kashmir.Github.Types.Organization
import           Kashmir.Github.Types.User
import           Network.Wreq

githubGet :: FromJSON a
          => URL -> AccessToken -> IO a
githubGet uri (AccessToken t) =
  do r <-
       asJSON =<<
       getWith (defaults & param "access_token" .~ [t])
               ("https://api.github.com" <> unpack uri)
     return (view responseBody r)

getUser :: AccessToken -> IO User
getUser = githubGet "/user"

getOrganizations :: AccessToken -> IO [Organization]
getOrganizations = githubGet "/user/orgs"


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
