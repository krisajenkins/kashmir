module Kashmir.ETL.Download (fetchJSONPage) where

import           Control.Lens
import           Data.Aeson
import           Data.Text    ()
import           Network.Wreq

fetchJSONPage :: FromJSON a => String -> Options -> IO a
fetchJSONPage url opts =
  do response <- getWith opts url
     page <- asJSON response
     return $ page ^. responseBody
