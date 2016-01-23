{-# LANGUAGE Arrows #-}
module Input (parseInput) where

import FRP.Yampa -- filterE
import FRP.Yampa.Utilities

import Graphics.UI.GLUT

import Types

-- Event Definition:
filterKeyDowns :: SF (Event Input) (Event Input)
filterKeyDowns = arr $ filterE ((==Down) . keyState)

keyIntegral :: Double -> SF (Event a) Double
keyIntegral a = let eventToSpeed (Event _) = a
                    eventToSpeed NoEvent   = 0
                in arr eventToSpeed >>> integral

-- Input
parseInput :: SF (Event Input) ParsedInput
parseInput = proc i -> do
    down     <- filterKeyDowns                  -< i
    aCount   <- countKey 'a'                    -< down
    dCount   <- countKey 'd'                    -< down
    returnA -< ParsedInput aCount dCount
    where countKey c  = filterE ((==(Char c)) . key) ^>> keyIntegral 1
