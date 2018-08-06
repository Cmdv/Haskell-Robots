module Game.Commands where

import           Control.Monad.Trans.State
import           Game.DefaultsInits
import           Game.Loop
import qualified Game.Texts as GT
import           Game.Types

-- check which command was issued
whichCommand :: String -> GameState -> IO ()
whichCommand str gs =
   case str of
     "run:all" -> commandRunAll gs
     "run:one" -> commandRunOne gs
     "robots" -> commandTotalRobots gs
     "help" -> putStrLn GT.showHelp
     "viewTurn" -> commandViewSpecificTurn gs
     "presents" -> commandPresents gs
     "houses" -> commandHouses gs
     _ -> putStrLn "unknown command sorry!"

-- run the whole simulation
commandRunAll :: GameState -> IO ()
commandRunAll gs = do
  (_, newState) <- runStateT gameLoop [gs]
  print newState
  putStrLn GT.successSim

-- run one move
commandRunOne :: GameState -> IO ()
commandRunOne gs = do
  (_, newState) <- runStateT gameLoop [defaultSingleMove gs]
  print newState
  putStrLn GT.successSim

-- select any turn number to view a snap shot of state at given turn
commandViewSpecificTurn :: GameState -> IO ()
commandViewSpecificTurn gs = do
  (_, newState) <- runStateT gameLoop [gs]
  putStrLn $ "Chose a number between 1-" ++ show (lastGameTurn newState)
  i <- getLine
  let rewound = drop (read i) newState
  case rewound of
    [] -> print "You asked for a state that is before the beginning of time!"
    (x : _) -> print x
  where
    lastGameTurn s = gameStateTurnNumber (last s)

-- run the whole simulation
commandTotalRobots :: GameState -> IO ()
commandTotalRobots gs = do
  (_, newState) <- runStateT gameLoop [gs]
  print $ robotLocation newState
  putStrLn GT.successRobot
  where
    robotLocation s = gameStateRobots (last s)

-- Total presents
commandPresents :: GameState -> IO ()
commandPresents gs = do
  (_, newState) <- runStateT gameLoop [gs]
  putStrLn $ "\n🎁 " ++ show (robotLocation newState) ++ " presents where delivered!!"
  where
    robotLocation s = do
      let houses = gameStateVisitedHouses (last s)
      foldr (\a b -> houseNumberOfDeliveries a + b ) 0 houses

-- Show houses with 'x' or more deliveries
commandHouses :: GameState -> IO ()
commandHouses gs = do
  (_, newState) <- runStateT gameLoop [gs]
  putStrLn "Chose a number show houses with 'x' or more presents delivered:"
  i <- getLine
  let results = filterHouses (read i) (allHouses newState)
  case results of
    [] -> print "There are no houses to show sorry!!"
    r -> print r
  where
    allHouses s = gameStateVisitedHouses (last s)
    filterHouses i = filter (\h -> houseNumberOfDeliveries h >= i)
