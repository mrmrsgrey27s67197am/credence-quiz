{-# LANGUAGE OverloadedStrings, Rank2Types #-}

module Template where

import Data.Text
import Data.Map (Map)
import qualified Data.Map as M
import Data.Aeson.Lens
import Control.Lens
import Data.Aeson.Types
import Data.Scientific
import Data.ByteString.Internal

data QuestionTemplate a =
  QT { description :: Text,
       query :: ByteString,
       compProp :: Traversal' Value a,
       nameProp :: Traversal' Value Text }

instance Show (QuestionTemplate a) where
  show QT{ description = s } = show s

exampleTemplate :: QuestionTemplate Scientific
exampleTemplate = QT "Which country has a larger population?"
                  "[{\"/location/statistical_region/population\": [{\"number\": null,\"year\": null,\"sort\": \"-year\",\"limit\": 1}], \"type\": \"/location/country\",\"name\": null}]"
                  (values.key "/location/statistical_region/population".key "number"._Number)
                  (values.key "name"._String)
