module DrawState (drawFromState) where

import Types
import Picture
import GameLogic
import Maps

pictureFromMaze :: Maze -> Picture
pictureFromMaze maze drawF = drawF'
  where
    drawF' c
      | tile == Wall = '#'
      | tile == Box = '$'
      | tile == Storage = '.'
      | otherwise = drawF c
      where
        tile = maze c

drawPlayer :: State -> Picture
drawPlayer state = pictureCharAt '@' pos 
  where
    pos = stPlayerPos state

drawFromState :: State -> Picture
drawFromState state
  | isWinning state = pictureFromString "Level completed!" (C (-8) 0) & pictureFromString text (C (-5) (-1))
    where
      movesNo = stMovesNo state
      movesText = "Moves: "
      text = movesText ++ show movesNo
drawFromState state = player & map
  where
    player = drawPlayer state 
    map = pictureFromMaze drawWithBoxes
    drawWithBoxes = addBoxes (stBoxes state) maze
    maze = stMap state