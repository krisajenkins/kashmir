{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Kashmir.Github.Api (getUser, requestAccess) where

import           Control.Lens
import           Data.ByteString      hiding (putStrLn)
import           Data.Monoid
import           Kashmir.Github.Types
import           Network.Wreq

getUser :: AccessToken -> IO User
getUser (AccessToken t) =
  do r :: Response User <- asJSON =<<
       getWith (defaults &
                param "access_token" .~
                [t])
               "https://api.github.com/user"
     putStrLn $ "getUser Response " <> show r
     return (view responseBody r)


-- TODO This doesn't handle a response of:
--  responseBody = "{\"error\":\"bad_verification_code\",\"error_description\":\"The code passed is incorrect or expired.\",\"error_uri\":\"https://developer.github.com/v3/oauth/#bad-verification-code\"}"
requestAccess :: Config -> ByteString -> IO AccessTokenResponse
requestAccess config code =
  do r :: Response AccessTokenResponse <-
       asJSON =<<
       postWith (defaults &
                 header "Accept" .~
                 ["application/json"])
                (view accessUrl config)
                ["code" := code
                ,"client_id" :=
                 view clientId config
                ,"client_secret" :=
                 view clientSecret config]
     return (view responseBody r)
