module DrawState (drawFromState) where

import Types
import Picture
import GameLogic
import Maps
import Textures

pictureFromMaze :: Maze -> Picture
pictureFromMaze maze drawF = drawF'
  where
    drawF' c
      | tile == Wall = wallTexture
      | tile == Box = boxTexture
      | tile == BoxOnStorage = boxStorageTexture
      | tile == Storage = storageTexture
      | tile == Ground = groundTexture
      | otherwise = drawF c
      where
        tile = maze c

drawPlayer :: State -> Picture
drawPlayer state
  | underPlayer == Ground = pictureCharAt playerTexture pos
  | underPlayer == Storage = pictureCharAt playerStorageTexture pos
  where
    pos = stPlayerPos state
    underPlayer = stMap state pos

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