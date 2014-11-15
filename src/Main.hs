
module Main where

import Logic
import Freebase
import Template

import Control.Monad.State
import Control.Lens

main :: IO ()
main = return ()

initiateManager :: Game ()
initiateManager = do
  man <- liftIO getManager
  manager .= man


runGame :: Game a -> IO (a, GameState)
runGame g = let g' = do initiateManager
                        g
            in runStateT (game g') initial
