module Game.Loop where

import Control.Monad.Trans.State
import Data.Traversable
import Game.Moves (robotDirToPos)
import Game.Types

gameLoop :: StateT [GameState] IO ()
gameLoop = do
  gameState <- get
  _ <- for (gameStateMoves $ head gameState)
      (\_ -> do
          gs <- get
          modify $ flip (<>) (singleGameLoop gs)
      )
  return ()

singleGameLoop :: [GameState] -> [GameState]
singleGameLoop ogs = do
  -- loop over the game moves
          let lastOldGameState = last ogs
              (m, r)           = head $ gameStateMoves lastOldGameState
              allRobots        = gameStateRobots lastOldGameState
              curRobot         = head $ filter (\a -> r == robotName a) allRobots
              curPos           = robotCurPosition curRobot
              newPos           = robotDirToPos m curPos
              newTurnNumber    = gameStateTurnNumber lastOldGameState + 1
              updateH          = updateHouses newPos (gameStateVisitedHouses lastOldGameState)
          -- is another robot already on the position current robot want's to move onto
          -- or is the robot going back to base (0,0)
          if newPos `elem` (robotCurPosition <$> allRobots) || newPos == (0,0)
            then
              [GameState
                { gameStateTurnNumber = newTurnNumber
                , gameStateRobots = updateRobot newPos curRobot allRobots 0
                , gameStateMoves = tail $ gameStateMoves lastOldGameState
                , gameStateVisitedHouses = updateH 0
                }]
            else
              [GameState
                { gameStateTurnNumber = newTurnNumber
                , gameStateRobots = updateRobot newPos curRobot allRobots 1
                , gameStateMoves = tail $ gameStateMoves lastOldGameState
                , gameStateVisitedHouses = updateH 1
                }]

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
