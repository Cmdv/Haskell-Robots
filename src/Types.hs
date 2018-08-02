module Types where

-- Positions of Robot / Houses
type Position = (Int, Int)


data Direction = MoveUp | MoveDown | MoveLeft | MoveRight deriving (Show, Eq)

-- All the values a Robot needs
data Robot = Robot { robotName :: String
                   , robotCurPosition :: Position
                   , robotDelivered :: Int
                   } deriving (Show, Eq)

-- House
data House = House { housePosition :: Position
                   , houseNumberOfDeliveries :: Int
                   } deriving (Show, Eq)

-- Game State
data GameState = GameState { gameStateTurnNumber :: Int
                           , gameStateRobots :: [Robot]
                           , gameStateMoves :: [(Direction, String)]
                           , gameStateVisitedHouses :: [House]
                           } deriving (Show, Eq)
