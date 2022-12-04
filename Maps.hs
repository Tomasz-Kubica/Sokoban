module Maps ( allMazes, allMaps ) where

import Types

mazes :: [Maze]
mazes = [maze, maze2, maze3]
maps :: [Map]
maps = [map1, map2, map3]

maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground
map1 = Map maze (C 0 (-1)) U

maze2 (C x y)
  | ax > 3 || ay > 5                 = Blank
  | ax == 3 || ay == 5               = Wall
  | max ax ay == 1 && min ax ay == 0 = Box
  | ax == 2 && ay == 4               = Storage
  | otherwise                        = Ground 
    where
      ax = abs x
      ay = abs y
map2 = Map maze2 (C 0 0) U

maze3 (C x y)
  | ax > 4 || ay > 6                 = Blank
  | ax == 4 || ay == 6               = Wall
  | max ax ay == 1 && min ax ay == 0 = Storage
  | ax == 2 && ay == 4               = Box
  | otherwise                        = Ground 
    where
      ax = abs x
      ay = abs y
map3 = Map maze3 (C 0 0) U

badMazes :: [Maze]
badMazes = [badMaze1, badMaze2, badMaze3]
badMaps :: [Map]
badMaps = [badMap1, badMap2, badMap3]


badMaze1 (C x y) -- Not closed
  | x == 0 && y == 0         = Storage
  | x == 2 && y == -1        = Box
  | max (abs x) (abs y) <= 3 = Ground
  | otherwise                = Blank
badMap1 = Map badMaze1 (C 0 0) U

badMaze2 (C x y) -- Boxes and storage separated
  | x == 0 && y == 0         = Box
  | x == 1 && y == 1         = Storage
  | max (abs x) (abs y) == 1 = Ground
  | max (abs x) (abs y) == 2 = Wall
  | max (abs x) (abs y) == 3 = Ground
  | max (abs x) (abs y) == 4 = Box
  | max (abs x) (abs y) == 5 = Ground
  | max (abs x) (abs y) == 6 = Wall
  | otherwise                = Blank
badMap2 = Map badMaze2 (C 0 (-1)) U

badMaze3 (C x y) -- Less storage then boxes
  | x == 0 && y == 1         = Storage
  | x == -1 && y == 0        = Box
  | x == 1 && y == 0         = Box
  | max (abs x) (abs y) <= 3 = Ground
  | max (abs x) (abs y) == 4 = Wall
  | otherwise                = Blank
badMap3 = Map badMaze3 (C 0 0) U

allMazes :: [Maze]
allMazes =  mazes ++ badMazes
allMaps :: [Map]
allMaps = maps ++ badMaps