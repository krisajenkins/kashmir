module Kashmir.Snap.Utils
       (requireParam, requirePostParam) where

import           Data.ByteString
import           Data.Monoid
import           Data.Text.Encoding
import           Database.Persist   ()
import           Prelude
import           Snap

requireParam :: MonadSnap m
             => ByteString -> m ByteString
requireParam name =
  do value <- getParam name
     case value of
       Just x -> return x
       Nothing ->
         do modifyResponse $
              setResponseStatus
                400
                (encodeUtf8 "Missing parameter: " <>
                 name)
            response <- getResponse
            finishWith response

requirePostParam :: MonadSnap m => ByteString -> m ByteString
requirePostParam name =
  do value <- getPostParam name
     case value of
       Just x -> return x
       Nothing ->
         do modifyResponse $
              setResponseStatus
                400
                (encodeUtf8 "Missing parameter: " <>
                 name)
            response <- getResponse
            finishWith response
