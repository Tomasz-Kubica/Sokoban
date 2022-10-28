{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
type Program = IO ()

main :: Program
main = drawingOf pictureOfMaze

tileSize :: Double
tileSize = 1

drawTile :: Int -> Picture
drawTile x
  | x == 1    = wall
  | x == 2    = ground
  | x == 3    = storage
  | x == 4    = box
  | otherwise = blank

solidTile = solidRectangle tileSize tileSize
wall = colored brown solidTile
ground = colored gray solidTile
storage = colored red (solidCircle (tileSize / 4)) & ground
boxLines = polyline [(-tileSize / 2, -tileSize / 2), (tileSize / 2, tileSize / 2)]
           & polyline [(-tileSize / 2, tileSize / 2), (tileSize / 2, -tileSize / 2)]
           & rectangle tileSize tileSize
box = boxLines & colored orange solidTile

type Maze = Int -> Int -> Int

maze :: Maze
maze x y
  | abs x > 4  || abs y > 4  = 0  -- blank
  | abs x == 4 || abs y == 4 = 1  -- wall
  | x ==  2 && y <= 0        = 1  -- wall
  | x ==  3 && y <= 0        = 3  -- storage
  | x >= -2 && y == 0        = 4  -- box
  | otherwise                = 2  -- ground


drawMaze :: Maze -> Int -> Int -> Int -> Int -> Picture
drawMaze maze x1 y1 x2 y2 = 
  translated (-width / 2) (height / 2) (drawMazeAux maze x1 y1 x2 y2)
  where 
    width = fromIntegral (x2 - x1) * tileSize
    height = fromIntegral (y2 - y1) * tileSize

drawMazeAux :: Maze -> Int -> Int -> Int -> Int -> Picture
drawMazeAux maze x1 y1 x2 y2
  | y2 >= y1  = drawMazeLine maze x1 x2 y2
                & translated 0 (-tileSize) (drawMazeAux maze x1 y1 x2 (y2 - 1))
  | otherwise = blank

drawMazeLine :: Maze -> Int -> Int -> Int -> Picture
drawMazeLine maze x l y
  | x > l     = blank
  | otherwise = drawTile (maze x y)
                & translated tileSize 0 (drawMazeLine maze (x + 1) l y)


pictureOfMaze :: Picture
pictureOfMaze = drawMaze maze (-10) (-10) 10 10