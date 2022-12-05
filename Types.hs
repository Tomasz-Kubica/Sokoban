module Types (
  Program, 
  Tile(..), 
  Direction(..), 
  Coord(..),
  Maze(..), 
  Map(..), 
  State(..),
  Screen,
  Event(..)
) where

type Program = IO ()

data Tile = Wall | Ground | Storage | Box | BoxOnStorage | Blank deriving Eq
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

data Event = KeyPress String

type Screen = String