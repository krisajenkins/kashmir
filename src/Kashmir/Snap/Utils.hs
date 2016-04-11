{-# LANGUAGE OverloadedStrings #-}
module Kashmir.Snap.Utils
       (requireParam, requirePostParam,requireBoundedJSON) where

-- TODO Merge Web and Snap.Utils.
import           Data.Aeson            as Aeson
import           Data.ByteString
import qualified Data.ByteString.Char8 as C
import           Data.Int
import           Data.Monoid
import           Data.Text.Encoding
import           Kashmir.Web
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

-------------------------------------------------------------------------------
-- | Demand the presence of JSON in the body with a size up to N
-- bytes. If parsing fails for any reson, request is terminated early
-- and a server error is returned.
requireBoundedJSON
    :: (MonadSnap m, FromJSON a)
    => Int64
    -> m a
requireBoundedJSON n =
  do res <- getBoundedJSON n
     case res of
       Left e -> malformedRequest e
       Right a -> return a

-------------------------------------------------------------------------------
-- | Parse request body into JSON or return an error string.
getBoundedJSON :: (MonadSnap m,FromJSON a)
               => Int64 -> m (Either ByteString a)
getBoundedJSON n =
  do bodyVal <- Aeson.decode `fmap` readRequestBody n
     return $
       case bodyVal of
         Nothing -> Left "Can't find JSON data in POST body"
         Just v ->
           case Aeson.fromJSON v of
             Aeson.Error e -> Left (C.pack e)
             Aeson.Success a -> Right a
