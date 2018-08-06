module Game.DefaultsInits where

import Data.Maybe
import Game.Moves (moveSeqToDirections, createRobots)
import Game.Types

defaultRobotArray :: [String]
defaultRobotArray = ["Robbie", "Jane", "Bob", "Becky", "Stephen", "Peter", "Suzie", "Alex", "Mary", "Hannah"]

getDefaultRobots :: Maybe Int -> [String]
getDefaultRobots n = case n of
  Nothing -> take 3 defaultRobotArray
  Just num -> take num defaultRobotArray

defaultMoveSequence :: String
defaultMoveSequence = "^^VV<>"

getDefaultMoves :: Maybe String -> String
getDefaultMoves =  fromMaybe defaultMoveSequence

initialGameState :: Maybe String -> Maybe Int -> GameState
initialGameState moves robotNum = do
  let robotNames = getDefaultRobots robotNum
      gameMoves  = getDefaultMoves moves
  GameState
    { gameStateTurnNumber = 0
    , gameStateRobots = createRobots robotNames
    , gameStateMoves = zip (moveSeqToDirections gameMoves) (cycle (robotName <$> createRobots robotNames))
    , gameStateVisitedHouses = []
    }

defaultSingleMove :: GameState -> GameState
defaultSingleMove gs = gs {gameStateMoves = [head $ gameStateMoves gs]}
