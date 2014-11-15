
module Main where

import Logic
import Freebase
import Template

import Control.Monad.State
import Control.Lens

main :: IO ()
main = do x <- runGame askQuestions
          putStrLn $ show x

askQuestions :: Game ()
askQuestions = do
  q <- generateQuestion exampleTemplate
  question q
  askQuestions


initiateManager :: Game ()
initiateManager = do
  man <- liftIO getManager
  manager .= man


runGame :: Game a -> IO (a, GameState)
runGame g = let g' = do initiateManager
                        g
            in runStateT (game g') initial
