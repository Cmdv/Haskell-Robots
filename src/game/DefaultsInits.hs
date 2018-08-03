module Game.DefaultsInits where

import Game.Moves (moveSeqToDirections, createRobots)
import Game.Types

defaultRobotArray :: [String]
defaultRobotArray = ["Robbie", "Jane", "Bob"]

defaultMoveSequence :: String
defaultMoveSequence = "^^VV<>"

initialGameState :: GameState
initialGameState = GameState
  { gameStateTurnNumber = 0
  , gameStateRobots = createRobots defaultRobotArray
  , gameStateMoves = zip (moveSeqToDirections defaultMoveSequence) (cycle (robotName <$> createRobots defaultRobotArray))
  , gameStateVisitedHouses = []
  }
