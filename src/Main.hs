module Main where

import           Control.Monad (when)
-- import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import qualified Game.Texts as GT
import           Game.Types
import qualified Game.Commands as C

viewRoundsAgo :: Int -> State [GameState] GameState
viewRoundsAgo i = do
  gameState <- get
  let rewound = drop i gameState
  case rewound of
    [] -> error "You asked for a state that is before the beginning of time!"
    (x : _) -> pure x

playGame :: (String, Int) -> IO ()
playGame (m,r) = do
  putStrLn GT.selectOption
  str <- getLine
  when (str /= "exit") $ do
    C.whichCommand str
    playGame (m,r)

main :: IO ()
main = do
  putStrLn GT.robotTitle
  mr <- GT.getMovesAndRobots
  playGame mr
  putStrLn GT.byeBye
