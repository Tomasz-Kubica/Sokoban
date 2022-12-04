{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- import Input
import Types
import Activity
-- import Picture
import Maps
import GameLogic
import DrawState

mainActivity = resettable (withStartScreen (withUndo activity))
  where
    activity = Activity startingState handleEvent drawFromState
    startingState = createStartState allMaps
  
main :: Program
main = runActivity mainActivity
