{-# LANGUAGE OverloadedStrings #-}

module Template where

import Data.Text

data QuestionTemplate =
  QT { description :: Text,
       freebaseType :: Text,
       compProp :: Text,
       count :: Int }
  deriving (Show, Eq)

exampleTemplate :: QuestionTemplate
exampleTemplate = QT "Who was born first?" "/people/person" "date_of_birth" 3385046
