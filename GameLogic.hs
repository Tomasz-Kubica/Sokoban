module GameLogic (
  handleEvent,
  createStartState,
  isWinning,
  addBoxes
) where

import Types
import Maps

maxMapLL = C (-10) (-10)
maxMapRU = C 10 10

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
addBoxes boxes maze c
  | c `elem` boxes && maze c == Storage = BoxOnStorage
  | c `elem` boxes = Box
  | otherwise = maze c

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

-- Game

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