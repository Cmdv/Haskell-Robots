module Game.Commands where

import           Control.Monad.Trans.State
import           Game.DefaultsInits
import           Game.Loop
import qualified Game.Texts as GT
import           Game.Types

-- check which command was issued
whichCommand :: String -> IO ()
whichCommand str =
   case str of
     "run:all" -> commandRunAll
     "robots" -> commandTotalRobots
     "help" -> putStrLn GT.showHelp
     "viewTurn" -> commandViewSpecificTurn
     _ -> putStrLn "unknown command sorry!"

-- run the whole simulation
commandRunAll :: IO ()
commandRunAll = do
  (_, newState) <- runStateT gameLoop [initialGameState]
  print newState
  putStrLn GT.success

-- select any turn number to view a snap shot of state at given turn
commandViewSpecificTurn :: IO ()
commandViewSpecificTurn = do
  (_, newState) <- runStateT gameLoop [initialGameState]
  putStrLn $ "Chose a number between 1-" ++ show (lastGameTurn newState)
  i <- getLine
  let rewound = drop (read i) newState
  case rewound of
    [] -> print "You asked for a state that is before the beginning of time!"
    (x : _) -> print x
  where
    lastGameTurn gs = gameStateTurnNumber (last gs)

-- run the whole simulation
commandTotalRobots :: IO ()
commandTotalRobots = do
  (_, newState) <- runStateT gameLoop [initialGameState]
  print $ robotLocation newState
  putStrLn GT.success
  where
    robotLocation gs = gameStateRobots (last gs)
