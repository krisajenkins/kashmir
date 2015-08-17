module Kashmir.Web where

import           Data.Aeson
import           Data.ByteString
import           Snap

mimeTypeJson :: ByteString
mimeTypeJson = "application/json"

jsonResponse :: MonadSnap m => m ()
jsonResponse = modifyResponse $ setContentType mimeTypeJson

writeJSON :: (ToJSON a,MonadSnap m)
          => a -> m ()
writeJSON a =
  do jsonResponse
     (writeLBS . encode) a
