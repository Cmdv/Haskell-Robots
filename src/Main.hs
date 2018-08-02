module Main where

import Control.Monad.Trans.State
import Data.Monoid ((<>))
import Data.Traversable
import Moves (moveSeqToDirections, robotDirToPos)
import Types

robotArray :: [String]
robotArray = ["Robbie", "Jane", "Bob"]

moveSequence :: String
moveSequence = "^^VV<>"

initialGameState :: GameState
initialGameState = GameState
  { gameStateRoundNumber = 0
  , gameStateRobots = initialRobots robotArray
  , gameStateMoves = zip (moveSeqToDirections moveSequence) (cycle (robotName <$> initialRobots robotArray))
  , gameStateVisitedHouses = []
  }

initialRobots :: [String] -> [Robot]
initialRobots = fmap createRobot
  where
    createRobot name' = Robot name' (0,0) 0

roundLoop :: State GameState GameState
roundLoop = do
  gameState <- get
  _ <- for (gameStateMoves gameState)
      (\(m,r) -> do
          newGameState <- get
          let allRobots = gameStateRobots newGameState
              curRobot  = head $ filter (\a -> r == robotName a) allRobots
              curPos    = robotCurPosition curRobot
              newPos    = robotDirToPos m curPos
          -- is another robot already on the position current robot want's to move onto
          -- or is the robot going back to base (0,0)
          if newPos `elem` (robotCurPosition <$> allRobots) || newPos == (0,0)
            then do
              put GameState { gameStateRoundNumber = gameStateRoundNumber newGameState
                            , gameStateRobots = updateRobot newPos curRobot allRobots 0
                            , gameStateMoves = tail $ gameStateMoves newGameState
                            , gameStateVisitedHouses = updateHouses newPos (gameStateVisitedHouses newGameState) 0
                            }
            else
              put GameState { gameStateRoundNumber = gameStateRoundNumber newGameState
                            , gameStateRobots = updateRobot newPos curRobot allRobots 1
                            , gameStateMoves = tail $ gameStateMoves newGameState
                            , gameStateVisitedHouses = updateHouses newPos (gameStateVisitedHouses newGameState ) 1
                            }
      )
  return gameState

updateRobot :: Position -> Robot -> [Robot] -> Int -> [Robot]
updateRobot p curRobot allRobots num =
  (\r -> if robotName r == robotName curRobot
         then Robot (robotName curRobot) p (robotDelivered curRobot + num)
         else r
  ) <$> allRobots

updateHouses :: Position -> [House] -> Int -> [House]
updateHouses pos allHouses num | null allHouses = [House pos num]
                               | pos == (0,0) = allHouses
                               | not $ isHouseThere allHouses pos = allHouses <> [House pos num]
                               | isHouseThere allHouses pos = (\r -> if housePosition r == pos
                                                                     then House pos (houseNumberOfDeliveries r + num)
                                                                     else r
                                                              ) <$> allHouses


isHouseThere :: [House] -> Position -> Bool
isHouseThere hs pos =  or $ (\r -> housePosition r == pos) <$> hs

addRound :: GameState -> State [GameState] ()
addRound roundState = do
  oldState <- get
  put (roundState : oldState)

viewRoundsAgo :: Int -> State [GameState] GameState
viewRoundsAgo i = do
  gameState <- get
  let rewound = drop i gameState
  case rewound of
    [] -> error "You asked for a state that is before the beginning of time!"
    (x : _) -> pure x

main :: IO ()
main = print (execState roundLoop initialGameState)
