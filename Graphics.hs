module Graphics (initGL, render) where

import Unsafe.Coerce (unsafeCoerce)
import FRP.Yampa.Vector3

import Graphics.UI.GLUT hiding (Level,Vector3(..),normalize)
import qualified Graphics.UI.GLUT as G(Vector3(..))

import Types

-- Colors
green  = Color4 0.8 1.0 0.7 0.9 :: Color4 GLdouble
red    = Color4 1.0 0.7 0.8 1.0 :: Color4 GLdouble

-- Initialize OpenGL
initGL :: IO ()
initGL = do
    getArgsAndInitialize
    initialDisplayMode $= [ WithDepthBuffer, DoubleBuffered ]
    createWindow "Ping Pong"
    depthFunc          $= Just Less
    clearColor         $= Color4 255 255 255 0
    light (Light 0)    $= Enabled
    lighting           $= Enabled
    lightModelAmbient  $= Color4 0.5 0.5 0.5 1
    diffuse (Light 0)  $= Color4 1 1 1 1
    blend              $= Enabled
    blendFunc          $= (SrcAlpha, OneMinusSrcAlpha)
    colorMaterial      $= Just (FrontAndBack, AmbientAndDiffuse)
    reshapeCallback    $= Just resizeScene
    return ()

-- Copied from reactive-glut
resizeScene :: Size -> IO ()
resizeScene (Size w 0) = resizeScene (Size w 1) -- prevent divide by zero
resizeScene s@(Size width height) = do
    -- putStrLn "resizeScene"
    viewport   $= (Position 0 0, s)
    matrixMode $= Projection
    loadIdentity
    perspective 45 (w2/h2) 1 1000
    matrixMode $= Modelview 0
    where
        w2 = half width
        h2 = half height
        half z = realToFrac z / 2

-- Render Shapes
renderShapeAt :: (Color4 GLdouble) -> Object -> Double -> Double -> Double -> IO()
renderShapeAt c s x y z = preservingMatrix $ do
    color c
    putStrLn ("x: " ++ show x ++ ", y: " ++ show y ++ ", z: " ++ show z)
    translate $ G.Vector3 (adjust x) (adjust y) (adjust z)
    renderObject Solid s
    where adjust :: Double -> GLdouble
          adjust v = unsafeCoerce (0.5 - 3.0 + v)

-- Render the game
render :: GameState -> IO ()
render (Game p bp _) = do
    clear [ ColorBuffer, DepthBuffer ]
    loadIdentity
    renderShapeAt red ballObj 100 100 100
    renderShapeAt green playerObj (vector3X bp) (vector3Y bp) (vector3Z bp)
    swapBuffers
    where ballObj = Sphere' 0.5 20 20
          playerObj = Teapot 2

-- ============================================================

{-|
render gs@(Game{ xpos, ypos, xvel, yvel, playerXPos, playerXVel }) = do
    clear [ ColorBuffer, DepthBuffer ]
    loadIdentity
    renderBall $ vector3 (0) (unsafeCoerce ypos) (-20)
    renderPlayer $ vector3 (unsafeCoerce playerXPos) (0) (-20)
    swapBuffers
    where size2 :: R
          size2 = (fromInteger $ 6)/2
          green  = Color4 0.8 1.0 0.7 0.9 :: Color4 R
          greenG = Color4 0.8 1.0 0.7 1.0 :: Color4 R
          red    = Color4 1.0 0.7 0.8 1.0 :: Color4 R
          renderShapeAt s p = preservingMatrix $ do
            translate $ G.Vector3 (0.5 - size2 + vector3X p)
                                  (0.5 - size2 + vector3Y p)
                                  (0.5 - size2 + vector3Z p)
            renderObject Solid s
          renderBall   = (color red >>) . (renderShapeAt $ Sphere' 0.5 20 20)
          renderPlayer   = (color green >>) . (renderShapeAt $ Cube 2)
-}
