{-# LANGUAGE OverloadedStrings #-}

module Freebase where

import System.IO
import Network.HTTP.Conduit
import Network.URI
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy.Internal as BLI
import Data.Aeson.Lens
import Data.Aeson.Types
import Control.Lens
import Control.Monad.State

import Logic


getManager :: IO Manager
getManager = newManager conduitManagerSettings


queryFreebase :: BI.ByteString -> Game (Maybe Value)
queryFreebase q = do
  man <- use manager
  liftIO $ do
    initReq <- parseUrl freebaseURL
    resp <- httpLbs (setQueryString [("query", Just q)] initReq) man
    return $ responseBody resp ^? key "result"


freebaseURL = "https://www.googleapis.com/freebase/v1/mqlread"

exampleQuery :: BI.ByteString
exampleQuery =
  "[{\"type\": \"/location/location\", \"name\": null, \"name~=\": \"london\"}]"
