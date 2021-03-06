module Graphics (initGL, render) where

import Unsafe.Coerce (unsafeCoerce)
import FRP.Yampa.Vector3

import Graphics.UI.GLUT hiding (Level,Vector3(..), normalize)
import qualified Graphics.UI.GLUT as G(Vector3(..))

import Types

-- Colors
green  = Color4 0.8 1.0 0.7 0.9 :: Color4 GLdouble
red    = Color4 1.0 0.7 0.8 1.0 :: Color4 GLdouble
failRed = Color4 1.0 0.0 0.0 1.0 :: Color4 GLdouble

-- Initialize OpenGL
initGL :: IO ()
initGL = do
    getArgsAndInitialize
    initialDisplayMode $= [ WithDepthBuffer, DoubleBuffered ]
    createWindow "Ping Pong"
    depthFunc          $= Just Less
    clearColor         $= Color4 0 0 0 0
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

-- To GLdouble
toGD :: Double -> GLdouble
toGD v = unsafeCoerce v

-- Render Objects
renderBall :: (Color4 GLdouble) -> Double -> Double -> Double -> IO()
renderBall c x y z = preservingMatrix $ do
    color c
    translate $ G.Vector3 (toGD x) (toGD y) (toGD z)
    renderObject Solid (Sphere' 0.25 20 20)

renderBox :: (Color4 GLdouble) -> Double -> Double -> Double -> Double -> Double -> IO()
renderBox c x y z w h = preservingMatrix $ do
    color c
    translate $ G.Vector3 (toGD x) (toGD y) (toGD z)
    scale (toGD w) (toGD h) (toGD w)
    renderObject Solid (Cube 1)

showFailMsg :: Bool -> IO()
showFailMsg True = do
    renderBox failRed 0 0 10 100 100
showFailMsg False = do
    return ()


-- Render the game
render :: GameState -> IO ()
render (Game p bp isEnd) = do
    clear [ ColorBuffer, DepthBuffer ]
    loadIdentity
    lookAt eyeAt centerAt upVec

    renderBall red (vector3X bp) (vector3Y bp) 0
--    putStrLn ("isEnd: " ++ show isEnd ++ ", y: " ++ show (vector3Y bp))
    renderBox green (vector3X p) (vector3Y p) 0 1 0.1 -- Player
    showFailMsg isEnd
    swapBuffers
    where eyeAt = Vertex3 (toGD 0.0) (toGD 0.0) (toGD 10.0)
          centerAt = Vertex3 (toGD 0.0) (toGD 0.0) (toGD 0.0)
          upVec = G.Vector3 (toGD 0.0) (toGD 1.0) (toGD 0.0)
