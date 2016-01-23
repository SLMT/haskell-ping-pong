{-# LANGUAGE Arrows, BangPatterns, NamedFieldPuns #-}
module GameLogic (gameUpdate) where

import FRP.Yampa
import FRP.Yampa.Vector3

import Types

gameUpdate :: SF ParsedInput GameState
gameUpdate = proc (ParsedInput aCount dCount) -> do
  ballPosX <- bouncing uniformMove 0 0.1 -< ()
  ballPosY <- bouncing uniformMove 0 0.25 -< ()
  let playerXPos = realToFrac (dCount - aCount)
  returnA -< Game (vector3 playerXPos (-3) 0) (vector3 ballPosX ballPosY 0) (vector3 0 0 0)

uniformMove :: Pos -> Vel -> SF () Pos
uniformMove p v = proc _ -> do
  newP <- integral >>^ (+ p) -< v
  returnA -< newP

bouncing :: (Pos -> Vel -> SF () Pos) -> Pos -> Vel -> SF () Pos
bouncing move p v = switch con (\pos -> bouncing move pos (-v))
  where con = proc input -> do
              newP <- move p v -< input
              event <- edge -< (newP <= -4 || newP >= 4)
              returnA -< (newP, event `tag` newP)
