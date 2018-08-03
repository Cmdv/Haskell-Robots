module Main where

import           Control.Monad (when)
import qualified Game.Commands as C
import           Game.DefaultsInits
import qualified Game.Texts as GT

playGame :: (Maybe String, Maybe Int) -> IO ()
playGame (moves, robotNumber) = do
  putStrLn GT.selectOption
  str <- getLine
  when (str /= "exit") $ do
    C.whichCommand str (initialGameState moves robotNumber)
    playGame (moves, robotNumber)

main :: IO ()
main = do
  putStrLn GT.robotTitle
  (moves, robotNum) <- GT.getMovesAndRobots
  playGame (moves, robotNum)
  putStrLn GT.byeBye
