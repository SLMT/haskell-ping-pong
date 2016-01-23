{-# LANGUAGE Arrows, BangPatterns, NamedFieldPuns #-}
module GameLogic (gameUpdate) where

import FRP.Yampa
import FRP.Yampa.Vector3

import Types

gameUpdate :: SF ParsedInput GameState
gameUpdate = proc (ParsedInput aCount dCount) -> do
  ballPos <- uniformMove (vector3 0 0 0) (vector3 0.3 0.3 0) -< ()
  let playerXPos = realToFrac (dCount - aCount)
  returnA -< Game (vector3 playerXPos (-3) 0) ballPos (vector3 0 0 0)

uniformMove :: Pos3 -> Vel3 -> SF () Pos3
uniformMove p v = proc _ -> do
  newPx <- integral >>^ (+ (vector3X p)) -< (vector3X v)
  newPy <- integral >>^ (+ (vector3Y p)) -< (vector3Y v)
  newPz <- integral >>^ (+ (vector3Z p)) -< (vector3Z v)
  returnA -< vector3 newPx newPy newPz

--
-- bouncing :: (Pos -> Vel -> SF () (Pos, Vel)) -> Pos -> Vel -> SF () (Pos, Vel)
-- bouncing move p0 v0 = switch (bX p0 v0) (\ (pos, vel) -> bouncing move pos (-vel))
--     where bX p0' v0' = proc input -> do
--             (pos, vel) <- move p0' v0' -< input
--             event <- edge -< (pos <= -5 || pos >= 10)
--             returnA -< ((pos, vel), event `tag` (pos, vel))
--
