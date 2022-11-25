{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}

import CodeWorld
import qualified Data.Text as T

-----------
-- Types --
-----------

type Program = IO ()

data Tile = Wall | Ground | Storage | Box | Blank deriving Eq
data Direction = R | U | L | D deriving Eq
data Coord = C Int Int deriving Eq

type Maze = Coord -> Tile

data Map = Map {
  mapMaze     :: Maze,
  mapStartPos :: Coord,
  mapStartDir :: Direction
}

data State = S {
  stPlayerPos  :: Coord,
  stPlayerDir  :: Direction,
  stMap        :: Maze,
  stBoxes      :: [Coord],
  stMovesNo    :: Int,
  stMaps       :: [Map],
  stCurrentMap :: Int
}

data SSState world = StartScreen | Running world
data WithUndo a = WithUndo a [a]

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

maxMapLL = C (-10) (-10)
maxMapRU = C 10 10

standardMapRadius = 8

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
mainActivity = resettable (withStartScreen (withUndo activity))
  where
    activity = Activity startingState handleEvent drawFromState
    startingState = createStartState allMaps
  
main :: Program
main = runActivity mainActivity

-- Activity
resettable :: Activity s -> Activity s
resettable (Activity world eventHandler draw) 
  = Activity world newEventHandler draw
  where
    newEventHandler (KeyPress "Esc") _ = world
    newEventHandler e w = eventHandler e w

withStartScreen :: Activity s -> Activity (SSState s)
withStartScreen (Activity state0 handle draw)
  = Activity state0' handle' draw'
  where
    state0' = StartScreen

    handle' (KeyPress " ") StartScreen = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s

withUndo :: Eq a => Activity a -> Activity (WithUndo a)
withUndo (Activity state0 handle draw) = Activity state0' handle' draw' where
    state0' = WithUndo state0 []
    handle' (KeyPress key) (WithUndo s stack) | key == "U"
      = case stack of 
        s':stack' -> WithUndo s' stack'
        []        -> WithUndo s []
    handle' e (WithUndo s stack)
       | s' == s = WithUndo s stack
       | otherwise = WithUndo (handle e s) (s:stack)
      where s' = handle e s
    draw' (WithUndo s _) = draw s

runActivity :: Activity s -> IO ()
runActivity (Activity world handle draw) = activityOf world handle draw

-- Basic polymorphic
foldList :: (a -> b -> b) -> b -> [a] -> b
foldList f b (h:t) = foldList f (f h b) t
foldList _ b [] = b

foldRList :: (a -> b -> b) -> b -> [a] -> b
foldRList f b (h:t) = f h (foldRList f b t)
foldRList _ b [] = b

elemList :: Eq a => a -> [a] -> Bool
elemList a = foldList (\x r -> x == a || r) False

appendList :: [a] -> [a] -> [a]
appendList l1 l2 = foldRList (:) l2 l1

listLength :: [a] -> Int
listLength = foldList (\_ n -> n + 1) 0

filterList :: (a -> Bool) -> [a] -> [a]
filterList f = foldRList (\x l -> if f x then x:l else l) []

nth :: [a] -> Int -> a
nth (h:t) 0 = h
nth (h:t) n = nth t (n - 1)

mapList :: (a -> b) -> [a] -> [b]
mapList f = foldRList (\a b -> f a : b) []

andList :: [Bool] -> Bool
andList = foldList f True
  where
    f a b = a && b

allList :: (a -> Bool) -> [a] -> Bool
allList f l = andList (mapList f l)

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

allAdjacentCoord :: Coord -> [Coord]
allAdjacentCoord c = mapList f [R, L, U, D]
  where
    f d = adjacentCoord d c

moveCoords :: [Direction] -> Coord -> Coord
moveCoords [] coord = coord
moveCoords (h:t) coord = adjacentCoord h coord

getTiles :: Tile -> Coord -> Coord -> Maze -> [Coord]
getTiles t (C x1 y1) (C x2 y2) maze
  | y1 > y2   = []
  | otherwise = 
    getTilesInLine t x1 x2 y1 maze ++ getTiles t (C x1 (y1 + 1)) (C x2 y2) maze

getTilesInLine :: Tile -> Int -> Int -> Int -> Maze -> [Coord]
getTilesInLine t x1 x2 y maze
  | x1 > x2   = []
  | otherwise = if maze c == t then c:otherTiles else otherTiles
  where
    otherTiles = getTilesInLine t (x1 + 1) x2 y maze
    c = C x1 y

getBoxes :: Coord -> Coord -> Maze -> [Coord]
getBoxes = getTiles Box

removeBoxes :: Maze -> Maze
removeBoxes maze = boxToGround . maze
  where
    boxToGround t = if t == Box then Ground else t

addBoxes :: [Coord] -> Maze -> Maze
addBoxes boxes maze c = if elem c boxes then Box else maze c

allUnique :: (Eq a) => [a] -> Bool
allUnique [] = True
allUnique (h:t) = notElem h t && allUnique t

-- Graph

isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighbours isOk = aux [initial] []
  where
    aux [] _ = True
    aux (x:toVisit) visited = isOk x && aux toVisit' visited'
      where
        newToVisit = filterList (\a -> not (elemList a visited')) (neighbours x)
        toVisit' = appendList newToVisit toVisit
        visited' = x:visited

reachable :: Eq a => a -> a -> (a -> [a]) -> Bool
reachable v initial neighbours = aux [initial] []
  where
    aux [] _ = False
    aux (x:toVisit) visited = x == v || aux toVisit' visited'
      where
        newToVisit = filterList (\a -> not (elemList a visited')) (neighbours x)
        toVisit' = appendList newToVisit toVisit
        visited' = x:visited

allReachable :: Eq a => [a] -> a -> (a -> [a]) -> Bool
allReachable vs initial neighbours = allList f vs
  where
    f x = reachable x initial neighbours

-- Levels

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
allMazes = appendList mazes badMazes
allMaps :: [Map]
allMaps = appendList maps badMaps

-- Verify levels

adjacentWalkable :: Maze -> Coord -> [Coord]
adjacentWalkable m c = filterList walkable all
  where
    all = allAdjacentCoord c
    walkable c = m c /= Wall

isClosed :: Maze -> Bool
isClosed m = allList checkInitial initial
  where
    initial = appendList storages grounds
    storages = getTiles Storage maxMapLL maxMapRU m
    grounds = getTiles Ground maxMapLL maxMapRU m
    checkInitial init = isGraphClosed init (adjacentWalkable m) isOk
    isOk c = m c /= Blank

isSane :: Maze -> Bool
isSane m = allList compareReachable initials
  where
    getT t = getTiles t maxMapLL maxMapRU m
    storages = getT Storage
    boxes = getT Box
    grounds = getT Ground
    initials = foldList appendList [] [storages, boxes, grounds]
    compareReachable initial = storagesNo >= boxesNo
      where
        reach c = reachable c initial (adjacentWalkable m)

        reachableStorages = filterList reach storages
        storagesNo = listLength reachableStorages

        reachableBoxes = filterList reach boxes
        boxesNo = listLength reachableBoxes

isCorrect :: Maze -> Bool
isCorrect m = isClosed m  && isSane m

-- Drawing level
drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

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
      rightAngle = pi / 2

drawPlayer :: State -> Picture
drawPlayer state = atCoord pos (rotated (angleFromDirection dir) player)
  where
    pos = stPlayerPos state
    dir = stPlayerDir state

-- Start screen

startScreen :: Picture
startScreen = pictureOfBools (map isCorrect allMazes)

sokobanTitle :: Picture
sokobanTitle = scaled 3 3 (lettering "Sokoban!")

pictureOfBools :: [Bool] -> Picture
pictureOfBools xs = translated (-fromIntegral k /2) (fromIntegral k) (go 0 xs)
  where n = length xs
        k = findK 0 -- k is the integer square of n
        findK i | i * i >= n = i
                | otherwise  = findK (i+1)
        go _ [] = blank
        go i (b:bs) =
          translated (fromIntegral (i `mod` k))
                     (-fromIntegral (i `div` k))
                     (pictureOfBool b)
          & go (i+1) bs

        pictureOfBool True =  colored green (solidCircle 0.4)
        pictureOfBool False = colored red   (solidCircle 0.4)
        
-- main = drawingOf(pictureOfBools (map even [1..49::Int]))

-- Game
drawFromState :: State -> Picture
drawFromState state
  | isWinning state = scaled 2 2 (lettering "Level completed!") & translated 0 (-5) (lettering text)
    where
      movesNo = stMovesNo state
      movesText = "Moves: "
      textChars = movesText ++ show movesNo
      text = T.pack textChars
drawFromState state = scaledMap
  where
    scaledMap = scaled scale scale unscaledMap

    scale = standardMapRadius / fromIntegral maxXY
    (C x1 y1) = maxMapLL
    (C x2 y2) = maxMapRU
    arrX = [x1..x2]
    arrY = [y1..y2]
    getMax x y (oldX, oldY)
      | maze (C x y) /= Blank = (x', y')
      | otherwise = (oldX, oldY)
      where
        x' = max (abs x) oldX
        y' = max (abs y) oldY
    maxes = mapList (\x -> foldList (getMax x) (0, 0) arrY) arrX
    (maxX, maxY) = foldList (\(x, y) z -> getMax x y z) (0, 0) maxes
    maxXY = max maxX maxY

    unscaledMap = player & map
    player = drawPlayer state 
    map = drawMaze drawWithBoxes (-10) (-10) 10 10
    drawWithBoxes = addBoxes (stBoxes state) maze

    maze = stMap state

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
isWinning state = all isBoxOk boxes
  where
    boxes = stBoxes state
    map = stMap state
    isStorage c = map c == Storage
    playerPos = stPlayerPos state
    adjacentWalkable c = filterList isGroundOrStorage all
      where
        all = allAdjacentCoord c
        isGroundOrStorage x = tile == Ground || tile == Storage
          where
            tile = map x
    reachableBox c = reachable c playerPos adjacentWalkable
    isBoxOk c = not (reachableBox c) || isStorage c


isLastMap :: State -> Bool
isLastMap s = currentMapNo == totalMapsNo - 1
  where
    totalMapsNo = listLength (stMaps s)
    currentMapNo = stCurrentMap s

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) s
  | isWinning s && key == " " && not (isLastMap s) = nexMapState s
  | isWinning s = s
handleEvent (KeyPress key) s
  | key == "N" && not (isLastMap s) = nexMapState s
  | isKeyDirection key = if playerOk && boxesOk then newS else s
    where 
      newS = s {
        stPlayerPos = newPlayerPos,
        stPlayerDir = dir,
        stBoxes = map (moveBox newPlayerPos dir) (stBoxes s),
        stMovesNo = stMovesNo s + 1
      }
      dir = directionFromKey key
      newPlayerPos = adjacentCoord dir (stPlayerPos s)

      playerOk = isPlayerOnFreeField newS
      boxesOk = areBoxesOk newS
handleEvent _ s = s

selectMapState :: State -> Int -> State
selectMapState s n = s {
  stMap = mazeWithoutBoxes,
  stPlayerPos = startPoss,
  stPlayerDir = startDir,
  stBoxes = startBoxes,
  stCurrentMap = n,
  stMovesNo = 0
}
  where
    maps = stMaps s
    map = nth maps n
    maze = mapMaze map
    startPoss = mapStartPos map
    startDir = mapStartDir map

    startBoxes = getBoxes maxMapLL maxMapRU maze
    mazeWithoutBoxes = removeBoxes maze

nexMapState :: State -> State
nexMapState s = selectMapState s mapNo
  where
    mapNo = stCurrentMap s + 1

createStartState :: [Map] -> State
createStartState maps = selectMapState tempS 0
  where
    tempS = S (C 0 0) U (const Blank) [] 0 maps 0

-- States equality

equalResult :: Eq a => (b -> a) -> b -> b -> Bool
equalResult f x y = f x == f y

instance Eq State where
  s1 == s2 = andList comparisons
    where
      comparisons = [
          equalResult stPlayerPos s1 s2,
          equalResult stPlayerDir s1 s2,
          equalResult stBoxes s1 s2,
          equalResult stMovesNo s1 s2
        ]
