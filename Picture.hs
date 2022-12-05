module Picture (Picture, blank, (&), drawPicture, pictureCharAt, pictureFromString) where

import Types

type DrawFun = Coord -> String
type Picture = DrawFun -> DrawFun

(&) :: Picture -> Picture -> Picture
(&) = (.)

blank :: Picture
blank = id

pictureCharAt :: String -> Coord -> Picture
pictureCharAt char at oldDraw coord
  | coord == at = char
  | otherwise   = oldDraw coord

pictureFromString :: String -> Coord -> Picture
pictureFromString str (C startX startY) oldDraw (C x y)
  | y == startY && x' >= 0 && x' < strLen = [str !! x']
  | otherwise = oldDraw (C x y)
  where
    x' = x - startX
    strLen = length str

maxX = 10
minX = -10
maxY = 10
minY = -10
drawToScreen draw = aux maxX maxY
  where
    aux x y
      | y < minY  = []
      | x > maxX  = "\n" : aux minX (y - 1)
      | otherwise = draw (C x y) : aux (x + 1) y


drawPicture :: Picture -> Screen
drawPicture picture = concat screen
  where
    screen = drawToScreen drawF
    drawF = picture empty
    empty c = " "