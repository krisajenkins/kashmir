{-# LANGUAGE OverloadedStrings #-}
module Kashmir.Web where

import           Data.Aeson
import           Data.ByteString
import           Snap

mimeTypeJson, mimeTypeCss :: ByteString
mimeTypeJson = "application/json"
mimeTypeCss = "text/css"

jsonResponse :: MonadSnap m => m ()
jsonResponse = modifyResponse $ setContentType mimeTypeJson

writeJSON :: (ToJSON a,MonadSnap m)
          => a -> m ()
writeJSON a =
  do jsonResponse
     (writeLBS . encode) a

handleError :: MonadSnap m => Int -> m b
handleError errorCode =
  do modifyResponse $ setResponseCode errorCode
     writeBS ""
     getResponse >>= finishWith

handleErrorWithMessage :: (MonadSnap m)
           => Int -> ByteString -> m b
handleErrorWithMessage code errorMessage =
  do modifyResponse $ setResponseCode code
     logError errorMessage
     writeBS errorMessage
     getResponse >>= finishWith

forbidden, unauthorized, notfound :: (MonadSnap m) => m b
forbidden = handleError 403
unauthorized = handleError 401
notfound = handleError 404

serverError, malformedRequest :: (MonadSnap m) => ByteString -> m b
serverError = handleErrorWithMessage 500
malformedRequest = handleErrorWithMessage 400
