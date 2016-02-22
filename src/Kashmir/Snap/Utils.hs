{-# LANGUAGE OverloadedStrings #-}
module Kashmir.Snap.Utils
       (requireParam, requirePostParam) where

import           Data.ByteString
import           Data.Monoid
import           Data.Text.Encoding
import           Database.Persist   ()
import           Prelude
import           Snap

mandatory
  :: MonadSnap m
  => (ByteString -> m (Maybe b)) -> ByteString -> m b
mandatory lookupFn name =
  do value <- lookupFn name
     case value of
       Just x -> return x
       Nothing ->
         do modifyResponse $
              setResponseStatus 400
                                (encodeUtf8 "Missing parameter: " <> name)
            response <- getResponse
            finishWith response

requireParam :: MonadSnap m
             => ByteString -> m ByteString
requireParam = mandatory getParam

requirePostParam :: MonadSnap m => ByteString -> m ByteString
requirePostParam = mandatory getPostParam
