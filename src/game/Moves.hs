module Game.Moves where

import Data.Maybe (mapMaybe)
import Game.Types

moveSeqToDirections :: String -> [Direction]
moveSeqToDirections = mapMaybe parse
  where
   parse letter =
      case letter of
      '^' -> Just MoveUp
      'V' -> Just MoveDown
      '<' -> Just MoveLeft
      '>' -> Just MoveRight
      _ -> Nothing

robotDirToPos :: Direction -> Position -> Position
robotDirToPos dir (x, y) = case dir of
  MoveRight -> (x + 1 , y)
  MoveLeft  -> (x - 1 , y)
  MoveDown  -> (x     , y - 1)
  MoveUp    -> (x     , y + 1)

createRobots :: [String] -> [Robot]
createRobots = fmap createRobot
  where
    createRobot name' = Robot name' (0,0) 0

updateRobotPos :: Position -> Robot -> [Robot] -> Int -> [Robot]
updateRobotPos p curRobot allRobots num =
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
