{-# LANGUAGE Arrows, BangPatterns, NamedFieldPuns #-}
module GameLogic (gameUpdate) where

import FRP.Yampa
import FRP.Yampa.Vector3

import Types

gameUpdate :: SF ParsedInput GameState
gameUpdate = proc (ParsedInput aCount dCount) -> do
    let playerXPos = realToFrac (dCount - aCount)
    returnA -< Game (vector3 playerXPos (-3) 0) (vector3 0 0 0) (vector3 0 0 0)
