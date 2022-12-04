
module Activity (
  Activity(..),
  runActivity,
  resettable,
  withStartScreen,
  withUndo
) where

import Data.Char (toUpper)
import System.IO
import Types
import Picture

data Activity world = Activity {
  actState  :: world,
  actHandle :: Event -> world -> world,
  actDraw   :: world -> Picture
}

-- Run activity

escChar = '\ESC'
unrecognizedKey = KeyPress "Unrecognized"

letterToArrow :: Char -> String
letterToArrow 'A' = "Up"
letterToArrow 'B' = "Down"
letterToArrow 'C' = "Right"
letterToArrow 'D' = "Left"
letterToArrow _   = "Unrecognized"

getEvent = do
  char <- getChar
  if char == escChar then do
    char2 <- getChar
    if char2 == '[' then do
      KeyPress . letterToArrow <$> getChar
    else
      return unrecognizedKey
  else
    return (KeyPress [toUpper char])

clear = putStr "\ESCc"

runActivity :: Activity s -> IO ()
runActivity activity = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  activityLoop activity
  where
    activityLoop (Activity world handle draw) = do
      let picture = draw world
      let screen = drawPicture picture
      clear
      putStr screen
      event <- getEvent
      let newWorld = handle event world
      activityLoop (Activity newWorld handle draw)


-- Resettable

resettable :: Activity s -> Activity s
resettable (Activity world eventHandler draw)
  = Activity world newEventHandler draw
  where
    newEventHandler (KeyPress "Esc") _ = world
    newEventHandler e w = eventHandler e w

-- Start screen

startScreen :: Picture
-- startScreen = pictureOfBools (map isCorrect allMazes)
startScreen = pictureFromString "Sokoban" (C 0 0)

-- sokobanTitle :: Picture
-- sokobanTitle = scaled 3 3 (lettering "Sokoban!")

-- pictureOfBools :: [Bool] -> Picture
-- pictureOfBools xs = translated (-fromIntegral k /2) (fromIntegral k) (go 0 xs)
--   where n = length xs
--         k = findK 0 -- k is the integer square of n
--         findK i | i * i >= n = i
--                 | otherwise  = findK (i+1)
--         go _ [] = blank
--         go i (b:bs) =
--           translated (fromIntegral (i `mod` k))
--                      (-fromIntegral (i `div` k))
--                      (pictureOfBool b)
--           & go (i+1) bs

--         pictureOfBool True =  colored green (solidCircle 0.4)
--         pictureOfBool False = colored red   (solidCircle 0.4)

data SSState world = StartScreen | Running world
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

-- Undo

data WithUndo a = WithUndo a [a]
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