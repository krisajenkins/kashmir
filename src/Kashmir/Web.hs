module Kashmir.Web where

import           Data.ByteString
import           Snap

mimeTypeJson :: ByteString
mimeTypeJson = "application/json"

jsonResponse :: MonadSnap m => m ()
jsonResponse = modifyResponse $ setContentType mimeTypeJson
