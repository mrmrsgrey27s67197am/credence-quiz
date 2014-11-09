
module Freebase where

import System.IO
import Network.HTTP.Conduit
import Network.URI
import Data.ByteString.Lazy


queryFreebase q = do
  initReq <- parseUrl freebaseURL
  man <- newManager conduitManagerSettings
  httpLbs (setQueryString [("query", Just q)] initReq) man


freebaseURL = "https://www.googleapis.com/freebase/v1/mqlread"
