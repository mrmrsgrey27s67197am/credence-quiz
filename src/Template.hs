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
import System.Random

import Freebase
import Logic

data QuestionTemplate a =
  QT { description :: Text,
       query :: ByteString,
       compProp :: Traversal' Value a,
       nameProp :: Traversal' Value Text,
       comparator :: a -> a -> Bool }

instance Show (QuestionTemplate a) where
  show QT{ description = s } = show s

chooseTwo :: [a] -> IO (a, a)
chooseTwo as = do
  a <- getStdRandom $ randomR (1, length as)
  b <- getStdRandom $ randomR (1, length as)
  if a == b
    then chooseTwo as
    else return (as !! pred a, as !! pred b)

deMaybe :: (Maybe a, Maybe b, Maybe c, Maybe d) -> Maybe (a, b, c, d)
deMaybe = undefined

generateQuestion :: QuestionTemplate a -> Game Question
generateQuestion qt = do
  resp <- queryFreebase $ query qt
  (a,b) <- chooseTwo $ resp ^.. _Just.values
  case deMaybe $ (a^?compProp qt, a^?nameProp qt,
                  b^?compProp qt, b^?nameProp qt) of
    Nothing -> generateQuestion qt
    Just (aVal, aName, bVal, bName) -> undefined


exampleTemplate :: QuestionTemplate Scientific
exampleTemplate = QT "Which country has a larger population?"
                  "[{\"/location/statistical_region/population\": [{\"number\": null,\"year\": null,\"sort\": \"-year\",\"limit\": 1}], \"type\": \"/location/country\",\"name\": null,\"limit\": 250}]"
                  (key "/location/statistical_region/population".values.key "number"._Number)
                  (key "name"._String)
                  (>)
