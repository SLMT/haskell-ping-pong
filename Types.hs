{-# LANGUAGE Arrows #-}
module Types where

import FRP.Yampa.Vector3

import Graphics.UI.GLUT hiding (Level,Vector3(..),normalize)
import qualified Graphics.UI.GLUT as G(Vector3(..))

-- Basic Data Types
type Pos3 = Vector3 Double
type Vel3 = Vector3 Double

-- Reference: Graphics.UI.GLUT.Callbacks.Window
data Input = Keyboard { key       :: Key, -- Char, SpecialKey or MouseButton
                        keyState  :: KeyState, -- Up or Down
                        modifiers :: Modifiers } -- shift, ctrl, alt

data ParsedInput = ParsedInput { aCount :: Double, dCount :: Double }

data GameState = Game { playerPos :: Pos3, ballPos :: Pos3, ballVel :: Vel3 }

{-|

type R = GLdouble

data Point3D = P3D { x :: Integer, y :: Integer, z :: Integer } deriving (Show)

data Level = Level { startingPoint :: Point3D,
                     endPoint      :: Point3D,
                     obstacles     :: [Point3D] }
-}
