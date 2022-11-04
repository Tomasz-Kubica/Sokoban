{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}

import CodeWorld

-----------
-- Types --
-----------

type Program = IO ()

data Tile = Wall | Ground | Storage | Box | Blank deriving Eq
data Direction = R | U | L | D deriving Eq
data Coord = C Int Int deriving Eq

type Maze = Coord -> Tile

data State = S {
  stPlayerPos :: Coord,
  stPlayerDir :: Direction,
  stMap       :: Maze,
  stBoxes     :: [Coord]
}

data SSState world = StartScreen | Running world

data Activity world = Activity {
  actState  :: world,
  actHandle :: Event -> world -> world,
  actDraw   :: world -> Picture
}

---------------
-- Constants --
---------------

tileSize :: Double
tileSize = 1

solidTile = solidRectangle tileSize tileSize
wall = colored brown solidTile
ground = colored gray solidTile
storage = colored red (solidCircle (tileSize / 4)) & ground
boxLines = polyline [(-tileSize / 2, -tileSize / 2), (tileSize / 2, tileSize / 2)]
           & polyline [(-tileSize / 2, tileSize / 2), (tileSize / 2, -tileSize / 2)]
           & rectangle tileSize tileSize
box = boxLines & colored orange solidTile
player = colored green (solidPolygon [
    (-tileSize / 3, -tileSize / 2), 
    (tileSize / 3, -tileSize / 2), 
    (0, tileSize / 2)
  ])

---------------
-- Functions --
---------------

-- Main
mainActivity = resettableActivityOf 
  (startScreenActivityOf 
    (Activity startingState handleEvent drawFromState)
  )
  where
    startingState = S startPoss startDir mazeWithoutBoxes startBoxes
    startBoxes = getBoxes (C (-10) (-10)) (C 10 10) maze
    mazeWithoutBoxes = removeBoxes maze
    startPoss = C 0 (-1)
    startDir = U
  
main :: Program
main = runActivity mainActivity

-- Activity
resettableActivityOf :: Activity s -> Activity s
resettableActivityOf (Activity world eventHandler draw) 
  = Activity world newEventHandler draw
  where
    newEventHandler (KeyPress "Esc") _ = world
    newEventHandler e w = eventHandler e w

startScreenActivityOf :: Activity s -> Activity (SSState s)
startScreenActivityOf (Activity state0 handle draw)
  = Activity state0' handle' draw'
  where
    state0' = StartScreen

    handle' (KeyPress " ") StartScreen = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s

runActivity :: Activity s -> IO ()
runActivity (Activity world handle draw) = activityOf world handle draw

-- Utility
atCoord :: Coord -> Picture -> Picture
atCoord (C x y) = translated 
  (fromIntegral x * tileSize) 
  (fromIntegral y * tileSize)

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x + 1) y
adjacentCoord L (C x y) = C (x - 1) y
adjacentCoord U (C x y) = C x (y + 1)
adjacentCoord D (C x y) = C x (y - 1)

moveCoords :: [Direction] -> Coord -> Coord
moveCoords [] coord = coord
moveCoords (h:t) coord = adjacentCoord h coord

getBoxes :: Coord -> Coord -> Maze -> [Coord]
getBoxes (C x1 y1) (C x2 y2) maze
  | y1 > y2   = []
  | otherwise = 
    getBoxesInLine x1 x2 y1 maze ++ getBoxes (C x1 (y1 + 1)) (C x2 y2) maze

getBoxesInLine :: Int -> Int -> Int -> Maze -> [Coord]
getBoxesInLine x1 x2 y maze
  | x1 > x2   = []
  | otherwise = if maze c == Box then c:otherBoxes else otherBoxes
  where
    otherBoxes = getBoxesInLine (x1 + 1) x2 y maze
    c = C x1 y

removeBoxes :: Maze -> Maze
removeBoxes maze = boxToGround . maze
  where
    boxToGround t = if t == Box then Ground else t

addBoxes :: [Coord] -> Maze -> Maze
addBoxes boxes maze c = if elem c boxes then Box else maze c

allUnique :: (Eq a) => [a] -> Bool
allUnique [] = True
allUnique (h:t) = (not (elem h t)) && (allUnique t)

-- Drawing level
drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

maze :: Maze
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground


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
  | otherwise = drawTile (maze (C x y))
                & translated tileSize 0 (drawMazeLine maze (x + 1) l y)

angleFromDirection :: Direction -> Double
angleFromDirection dir
  | dir == U = 0
  | dir == L = rightAngle
  | dir == D = rightAngle * 2
  | dir == R = rightAngle * 3
    where
      rightAngle = 1.5707963268

drawPlayer :: State -> Picture
drawPlayer state = atCoord pos (rotated (angleFromDirection dir) player)
  where
    pos = stPlayerPos state
    dir = stPlayerDir state

-- Start screen
startScreen :: Picture
startScreen = scaled 3 3 (lettering "Sokoban!")

-- Game
drawFromState :: State -> Picture
drawFromState state
  | isWinning state = scaled 3 3 (lettering "You won!")
drawFromState state = player & map
  where
    player = drawPlayer state 
    map = drawMaze drawWithBoxes (-10) (-10) 10 10
    drawWithBoxes = addBoxes (stBoxes state) (stMap state)

isKeyDirection key = elem key ["Up", "Down", "Right", "Left"]

directionFromKey key
  | key == "Up"    = U
  | key == "Down"  = D
  | key == "Right" = R
  | key == "Left"  = L

isFiledFree :: Maze -> Coord -> Bool
isFiledFree maze c = elem (maze c) freeFieldsTypes
  where
    freeFieldsTypes = [Ground, Storage]

isPlayerOnFreeField :: State -> Bool
isPlayerOnFreeField state =  isFiledFree (stMap state) (stPlayerPos state)

moveBox :: Coord -> Direction -> Coord -> Coord
moveBox cord dir box = if cord == box then adjacentCoord dir box else box

areBoxesOk :: State -> Bool
areBoxesOk state = allUnique boxes && all (isFiledFree (stMap state)) boxes
  where
    boxes = stBoxes state

isWinning :: State -> Bool
isWinning state = all isStorage boxes
  where
    boxes = stBoxes state
    map = stMap state
    isStorage c = map c == Storage

handleEvent :: Event -> State -> State
handleEvent _ s
  | isWinning s = s
handleEvent (KeyPress key) s
  | isKeyDirection key = if playerOk && boxesOk then newS else s
    where 
      newS = s {
        stPlayerPos = newPlayerPos,
        stPlayerDir = dir,
        stBoxes = map (moveBox newPlayerPos dir) (stBoxes s)
      }
      dir = directionFromKey key
      newPlayerPos = adjacentCoord dir (stPlayerPos s)

      playerOk = isPlayerOnFreeField newS
      boxesOk = areBoxesOk newS
handleEvent _ s = s
