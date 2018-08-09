module Game.Commands where

import           Control.Monad.Trans.State
import           Game.DefaultsInits
import           Game.Loop
import qualified Game.Texts as GT
import           Game.Types


runGameState :: [GameState] -> IO [GameState]
runGameState gss = do
  (_, newState) <- runStateT gameLoop gss
  return newState

-- Run the command that was given via command line
whichCommand :: String -> GameState -> IO ()
whichCommand str gs =
   case str of

     -- run the whole simulation
     "run:all" -> do
       print =<< runGameState [gs]
       putStrLn GT.successSim

     -- run the first move
     "run:one" -> do
       print =<< runGameState [defaultSingleMove gs]
       putStrLn GT.successSim

     -- show the status of robots after simulation
     "robots" -> do
       print . robotLocation =<< runGameState [gs]
       putStrLn GT.successRobot
         where
           robotLocation s = gameStateRobots (last s)

     -- show the help options
     "help" -> putStrLn GT.showHelp

     -- select any turn number to view a snap shot of state at given turn
     "viewTurn" -> do
        stateValue <- runGameState [gs]
        putStrLn $ "Chose a number between 1-" ++ show (lastGameTurn stateValue)
        i <- getLine
        let rewound = drop (read i) stateValue
        case rewound of
          [] -> print "You asked for a state that is before the beginning of time!"
          (x : _) -> print x
        where
          lastGameTurn s = gameStateTurnNumber (last s)

     -- total presents
     "presents" -> do
        stateValue <- runGameState [gs]
        putStrLn $ "\n🎁 " ++ show (robotLocation stateValue) ++ " presents where delivered!!"
        where
          robotLocation s = do
            let houses = gameStateVisitedHouses (last s)
            foldr (\a b -> houseNumberOfDeliveries a + b ) 0 houses

     -- Show houses with 'x' or more deliveries
     "houses" -> do
        stateValue <- runGameState [gs]
        putStrLn "Chose a number show houses with 'x' or more presents delivered:"
        i <- getLine
        let results = filterHouses (read i) (allHouses stateValue)
        case results of
          [] -> print "There are no houses to show sorry!!"
          r -> print r
          where
            allHouses s = gameStateVisitedHouses (last s)
            filterHouses i = filter (\h -> houseNumberOfDeliveries h >= i)

     _ -> putStrLn "unknown command sorry!"
