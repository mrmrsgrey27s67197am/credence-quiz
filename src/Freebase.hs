{-# LANGUAGE OverloadedStrings #-}

module Freebase where

import System.IO
import Network.HTTP.Conduit
import Network.URI
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy.Internal as BLI


queryFreebase q = do
  initReq <- parseUrl freebaseURL
  man <- newManager conduitManagerSettings
  httpLbs (setQueryString [("query", Just q)] initReq) man


freebaseURL = "https://www.googleapis.com/freebase/v1/mqlread"

exampleQuery :: BI.ByteString
exampleQuery = "[{\"type\": \"/location/location\", \"name\": null, \"name~=\": \"london city\"}]"
