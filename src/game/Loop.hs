module Game.Loop where

import Control.Monad.Trans.State
import Data.Traversable
import Game.Moves (robotDirToPos, updateRobotPos, updateHouses)
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
                , gameStateRobots = updateRobotPos newPos curRobot allRobots 0
                , gameStateMoves = tail $ gameStateMoves lastOldGameState
                , gameStateVisitedHouses = updateH 0
                }]
            else
              [GameState
                { gameStateTurnNumber = newTurnNumber
                , gameStateRobots = updateRobotPos newPos curRobot allRobots 1
                , gameStateMoves = tail $ gameStateMoves lastOldGameState
                , gameStateVisitedHouses = updateH 1
                }]
