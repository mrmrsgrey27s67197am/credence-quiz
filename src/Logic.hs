{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Logic where

import Data.Map (Map)
import qualified Data.Map as M
import Control.Lens
import Control.Applicative
import Control.Monad.State

data Question = Question String Bool
  deriving (Show, Eq)

  deriving (Show, Eq)

data GameState = GameState { _answers :: Map Int Int,
                             _manager :: Manager }

makeLenses ''GameState

newtype Game a = Game (StateT GameState IO a)
  deriving (Functor, Applicative, Monad, MonadState GameState)

initial :: GameState
initial = undefined & answers .~ M.empty

question :: Question -> Int -> Game ()
question (Question _ answer) credence =
  if answer
  then answers.at credence.non 0 += 1
  else answers.at (100 - credence).non 0 += 1

countScore :: Map Int Int -> Double
countScore m = let l = M.toList m
               in sum $ flip map l $ \(cr, n) ->
                    100 * fromIntegral n * logBase 2 (fromIntegral cr / 50)


score :: Game Double
score = uses answers countScore

