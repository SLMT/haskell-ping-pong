{-# LANGUAGE Arrows, BangPatterns, NamedFieldPuns #-}
module GameLogic (gameMain) where

import FRP.Yampa
import FRP.Yampa.Vector3

import Types

gameMain :: SF ParsedInput GameState
gameMain = switch con (\state -> gameEnd state)
    where
        con = proc input -> do
            state <- gameUpdate -< input
            event <- edge -< (vector3Y (ballPos state)) <= -3.8
            returnA -< (state, event `tag` state)

gameUpdate :: SF ParsedInput GameState
gameUpdate = proc (ParsedInput aCount dCount) -> do
    playerXPos <- controlPlayer -< (aCount, dCount)
    ballPosX <- ballMoveX uniformMove 0 0.1 -< ()
    ballPosY <- ballMoveY uniformMove 0 0.25 -< (ballPosX, playerXPos)
    returnA -< Game (vector3 playerXPos (-3) 0) (vector3 ballPosX ballPosY 0) False
    where
        controlPlayer = proc (ac, dc) -> do
            returnA -< realToFrac (dc - ac) * 0.3

uniformMove :: Pos -> Vel -> SF () Pos
uniformMove p v = proc _ -> do
    newP <- integral >>^ (+ p) -< v
    returnA -< newP

ballMoveX :: (Pos -> Vel -> SF () Pos) -> Pos -> Vel -> SF () Pos
ballMoveX move p v = switch con (\pos -> ballMoveX move pos (-v))
    where
        con = proc input -> do
            newP <- move p v -< ()
            event <- edge -< (newP <= -4 || newP >= 4)
            returnA -< (newP, event `tag` newP)

ballMoveY :: (Pos -> Vel -> SF () Pos) -> Pos -> Vel -> SF (Pos, Pos) Pos
ballMoveY move bpy v = switch con (\pos -> ballMoveY move pos (-v))
    where
        con = proc (bpx, ppx) -> do
            newP <- move bpy v -< ()
            event <- edge -< (newP <= -4 || newP >= 4 || (abs(bpx - ppx) <= 0.5 && newP <= -2.8))
            returnA -< (newP, event `tag` newP)

gameEnd :: GameState -> SF ParsedInput GameState
gameEnd lastState = proc input -> do
    returnA -< Game (playerPos lastState) (ballPos lastState) True
