
module Logic where

import qualified Data.IntMap as M

data Question = Question String Bool
  deriving (Show, Eq)

data Game = Game [(Int, Bool)]
  deriving (Show, Eq)

newGame :: Game
newGame = Game []

question :: Question -> Int -> Game -> Game
question (Question _ answer) credence (Game results) = Game ((credence, answer) : results)

score :: Game -> Double
score (Game []) = 0
score (Game ((credence, answer):rest)) = (firstScore * 100) + score (Game rest)
  where normalise = (/100) . fromIntegral
        flip = if answer then id else \x -> (1 - x)
        firstScore = logBase 2 (flip (normalise credence) / 0.5)
