module Main where

import Control.Monad.Trans.State
import Data.Monoid ((<>))
import Data.Traversable
import Moves (moveSeqToDirections, robotDirToPos)
import Types

defaultRobotArray :: [String]
defaultRobotArray = ["Robbie", "Jane", "Bob"]

defaultMoveSequence :: String
defaultMoveSequence = "^^VV<>"

initialGameState :: GameState
initialGameState = GameState
  { gameStateRoundNumber = 0
  , gameStateRobots = initialRobots defaultRobotArray
  , gameStateMoves = zip (moveSeqToDirections defaultMoveSequence) (cycle (robotName <$> initialRobots defaultRobotArray))
  , gameStateVisitedHouses = []
  }

initialRobots :: [String] -> [Robot]
initialRobots = fmap createRobot
  where
    createRobot name' = Robot name' (0,0) 0

roundLoop :: State [GameState] [GameState]
roundLoop = do
  gameState <- get
  -- loop over the game moves
  _ <- for (gameStateMoves $ last gameState)
      (\(m,r) -> do
          ogs <- get
          let lastOldGameState = last ogs
              allRobots      = gameStateRobots lastOldGameState
              curRobot       = head $ filter (\a -> r == robotName a) allRobots
              curPos         = robotCurPosition curRobot
              newPos         = robotDirToPos m curPos
              newroundNumber = gameStateRoundNumber lastOldGameState + 1
              updateH        = updateHouses newPos (gameStateVisitedHouses lastOldGameState)
          -- is another robot already on the position current robot want's to move onto
          -- or is the robot going back to base (0,0)
          if newPos `elem` (robotCurPosition <$> allRobots) || newPos == (0,0)
            then
              put $ ogs <> [GameState
                            { gameStateRoundNumber = newroundNumber
                            , gameStateRobots = updateRobot newPos curRobot allRobots 0
                            , gameStateMoves = tail $ gameStateMoves lastOldGameState
                            , gameStateVisitedHouses = updateH 0
                            }]
            else
              put $ ogs <> [GameState
                            { gameStateRoundNumber = newroundNumber
                            , gameStateRobots = updateRobot newPos curRobot allRobots 1
                            , gameStateMoves = tail $ gameStateMoves lastOldGameState
                            , gameStateVisitedHouses = updateH 1
                            }]
      )
  return gameState

updateRobot :: Position -> Robot -> [Robot] -> Int -> [Robot]
updateRobot p curRobot allRobots num =
  (\r -> if robotName r == robotName curRobot
         then Robot (robotName curRobot) p (robotDelivered curRobot + num)
         else r
  ) <$> allRobots

-- create/update a house to houses
updateHouses :: Position -> [House] -> Int -> [House]
updateHouses pos allHouses num | null allHouses = [House pos num]
                               | pos == (0,0) = allHouses
                               | not $ isHouseThere allHouses pos = allHouses <> [House pos num]
                               | isHouseThere allHouses pos =
                                   (\r -> if housePosition r == pos
                                          then House pos (houseNumberOfDeliveries r + num)
                                          else r
                                   ) <$> allHouses
                               | otherwise = allHouses


-- check if a house Position alread exists
isHouseThere :: [House] -> Position -> Bool
isHouseThere hs pos =  or $ (\r -> housePosition r == pos) <$> hs


viewRoundsAgo :: Int -> State [GameState] GameState
viewRoundsAgo i = do
  gameState <- get
  let rewound = drop i gameState
  case rewound of
    [] -> error "You asked for a state that is before the beginning of time!"
    (x : _) -> pure x

main :: IO ()
main = print $ last (execState roundLoop [initialGameState])
