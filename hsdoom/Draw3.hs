module Draw3 (
    d3dmain
    )where
import Graphics.UI.GLUT
import Graphics.GLUtil (readTexture)
import qualified Graphics.UI.GLUT.Objects as O
import Control.Monad
import Data.IORef
import TexDraw (drawTexturedQuad, TexturedQuad(..))
data Camera = Camera {
    camX    :: GLfloat,
    camY    :: GLfloat,
    camZ    :: GLfloat,
    camAngY :: GLfloat
}

d3dmain :: IO ()
d3dmain = do
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "Hello World"
    initialDisplayMode $= [WithDepthBuffer]--, DoubleBuffered] -- [WithDepthBuffer] 
    reshapeCallback    $= Just reshape
    depthFunc          $= Just Less
    texture Texture2D  $= Enabled
    cam                <- newIORef Camera { camX = 0.0, camY = 0.0, camZ = 0.5, camAngY = 0 }
    setupGeometry      cam
    Right tex          <- readTexture "C:\\Users\\user\\flats\\img4.png"
    
    keyboardMouseCallback $= Just (keyboardMouse cam)
    idleCallback       $= Just (idle cam)
    displayCallback    $= display cam tex
    windowSize         $= Size 480 480
    mainLoop

setupGeometry :: IORef Camera -> IO ()
setupGeometry cam = do
   matrixMode  $= Projection
   perspective 50.0 1.0 0.3 10.0
   matrixMode  $= Modelview 0
   translate   $ Vector3 0.0 0.0 (-3.0 :: GLfloat)

reshape :: ReshapeCallback
reshape size = do 
    viewport $= (Position 0 0, size)
 
keyboardMouse :: IORef Camera -> KeyboardMouseCallback
keyboardMouse cam key Down _ _ = case key of
  {-(SpecialKey KeyLeft ) -> p $~! \(x,y,z) -> (x-0.1,y)
  (SpecialKey KeyRight) -> p $~! \(x,y,z) -> (x+0.1,y)
  (SpecialKey KeyUp   ) -> p $~! \(x,y,z) -> (x,y+0.1)
  (SpecialKey KeyDown ) -> p $~! \(x,y,z) -> (x,y-0.1)-}
  _ -> return ()
keyboardMouse _ _ _ _ _ = return ()

points :: Int -> [(GLfloat,GLfloat,GLfloat)]
points n = [ (sin (2*pi*k/n'), cos (2*pi*k/n'), 0) | k <- [1..n'] ]
    where n' = fromIntegral n

--display :: IORef Camera -> IORef -> DisplayCallback
display cam tex = do 
    clear [ColorBuffer, DepthBuffer] -- clear depth buffer, too
    --loadIdentity
    {-(x',y') <- get pos
    translate $ Vector3 x' y' 0
    preservingMatrix $ do
      --a <- get angle
      --rotate a $ Vector3 0 0 1
      --rotate a $ Vector3 0 0.1 1 -- changed y-component a bit to show off cube corners
      translate  $ Vector3 0.5 0.0 (0.5 :: GLfloat)
      O.renderObject O.Wireframe (O.Teapot 1.0)
      --scale 0.8 0.8 (0.8::GLfloat)
      frameFloor 20-}
    drawWalls tex
    swapBuffers 
 
idle :: IORef Camera -> IdleCallback
idle cam = do
  {-d <- get delta
  angle $~! (+ d)-}
  postRedisplay Nothing

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z
 
cube :: GLfloat -> IO ()
cube w = renderPrimitive Quads $ mapM_ vertex3f
    [   ( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w),
        ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w),
        ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w),
        (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w),
        ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w),
        ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w) ]

cubeFrame :: GLfloat -> IO ()
cubeFrame w = renderPrimitive Lines $ mapM_ vertex3f
  [ ( w,-w, w), ( w, w, w),  ( w, w, w), (-w, w, w),
    (-w, w, w), (-w,-w, w),  (-w,-w, w), ( w,-w, w),
    ( w,-w, w), ( w,-w,-w),  ( w, w, w), ( w, w,-w),
    (-w, w, w), (-w, w,-w),  (-w,-w, w), (-w,-w,-w),
    ( w,-w,-w), ( w, w,-w),  ( w, w,-w), (-w, w,-w),
    (-w, w,-w), (-w,-w,-w),  (-w,-w,-w), ( w,-w,-w) ]


frameFloor :: Int -> IO ()
frameFloor n = renderPrimitive Lines $ mapM_ vertex3f vlist
    where 
        vlist = 
            concat [ [(fi x, 0.0, fi (-n)), (fi x, fi n, 0.0)] | x <- [(-n) .. n]]
            ++
            concat [ [(fi (-n), 0.0, fi y), (fi n, fi y, 0.0)] | y <- [(-n) .. n]]
        fi = fromIntegral



drawWalls :: TextureObject -> IO ()
drawWalls tex = do
  textureBinding Texture2D $= Just tex
  textureFilter  Texture2D $= ((Linear', Nothing), Linear')
  textureFunction          $= Replace
  forM_ [leftWall, frontWall, rightWall] drawTexturedQuad

leftWall =
  let (x1, x2, y1, y2, z1, z2) = (-0.9, -0.9, -0.6, 0.6, 0.6, -1.0)
      (tx1, tx2, ty1, ty2) = (3, 0, 1, 0) in
  wallTemplate x1 x2 y1 y2 z1 z2 tx1 tx2 ty1 ty2

frontWall =
  let (x1, x2, y1, y2, z1, z2) = (0.8, -0.8, -0.6, 0.6, -1.0, -1.0)
      (tx1, tx2, ty1, ty2) = (3, 0, 4, 0) in
  wallTemplate x1 x2 y1 y2 z1 z2 tx1 tx2 ty1 ty2

rightWall =
  let (x1, x2, y1, y2, z1, z2) = (0.9, 0.9, -0.6, 0.6, 0.6, -1.0)
      (tx1, tx2, ty1, ty2) = (0.9, 0.1, 0.9, 0.1) in
  wallTemplate x1 x2 y1 y2 z1 z2 tx1 tx2 ty1 ty2

wallTemplate x1 x2 y1 y2 z1 z2 tx1 tx2 ty1 ty2 =
  TexturedQuad x1 y1 z1 x1 y2 z1 x2 y2 z2 x2 y1 z2 tx1 tx2 ty1 ty2