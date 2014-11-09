{-# LANGUAGE OverloadedStrings #-}

module Freebase where

import System.IO
import Network.HTTP.Conduit
import Network.URI
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy.Internal as BLI



getManager :: IO Manager
getManager = newManager conduitManagerSettings


queryFreebase :: Manager -> BI.ByteString -> IO BLI.ByteString
queryFreebase man q = do
  initReq <- parseUrl freebaseURL
  resp <- httpLbs (setQueryString [("query", Just q)] initReq) man
  return $ responseBody resp


freebaseURL = "https://www.googleapis.com/freebase/v1/mqlread"

exampleQuery :: BI.ByteString
exampleQuery = "[{\"type\": \"/location/location\", \"name\": null, \"name~=\": \"london city\"}]"
