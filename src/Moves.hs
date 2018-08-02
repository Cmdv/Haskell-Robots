module Moves where

import Data.Maybe (mapMaybe)
import Types

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
