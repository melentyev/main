module Draw3 (
    d3dmain
    )where
import Graphics.UI.GLUT
import Control.Monad
import Data.IORef

d3dmain :: IO ()
d3dmain = do
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer]--, DoubleBuffered] -- [WithDepthBuffer] 
    _window <- createWindow "Hello World"
    reshapeCallback $= Just reshape
    depthFunc $= Just Less
    angle <- newIORef 0.0
    delta <- newIORef 0.0125
    pos <- newIORef (0, 0)
    keyboardMouseCallback $= Just (keyboardMouse delta pos)
    idleCallback $= Just (idle angle delta)
    displayCallback $= display angle pos
    mainLoop

reshape :: ReshapeCallback
reshape size = do 
    viewport $= (Position 0 0, size)
 
keyboardMouse :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> KeyboardMouseCallback
keyboardMouse a p key Down _ _ = case key of
  (Char ' ') -> a $~! negate
  (Char '+') -> a $~! (* 2)
  (Char '-') -> a $~! (/ 2)
  (SpecialKey KeyLeft ) -> p $~! \(x,y) -> (x-0.1,y)
  (SpecialKey KeyRight) -> p $~! \(x,y) -> (x+0.1,y)
  (SpecialKey KeyUp   ) -> p $~! \(x,y) -> (x,y+0.1)
  (SpecialKey KeyDown ) -> p $~! \(x,y) -> (x,y-0.1)
  _ -> return ()
keyboardMouse _ _ _ _ _ _ = return ()

points :: Int -> [(GLfloat,GLfloat,GLfloat)]
points n = [ (sin (2*pi*k/n'), cos (2*pi*k/n'), 0) | k <- [1..n'] ]
    where n' = fromIntegral n

display :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> DisplayCallback
display angle pos = do 
  clear [ColorBuffer, DepthBuffer] -- clear depth buffer, too
  clear [ColorBuffer]
  loadIdentity
  (x',y') <- get pos
  translate $ Vector3 x' y' 0
  preservingMatrix $ do
    a <- get angle
    rotate a $ Vector3 0 0 1
    rotate a $ Vector3 0 0.1 1 -- changed y-component a bit to show off cube corners
    scale 0.8 0.8 (0.8::GLfloat)
    forM_ (points 7) $ \(x,y,z) -> preservingMatrix $ do
      color $ Color3 ((x+1)/2) ((y+1)/2) ((z+1)/2)
      translate $ Vector3 x y z
      cube 0.2
      color $ Color3 (0::GLfloat) 0 0 -- set outline color to black
      cubeFrame 0.2 -- draw the outline
  swapBuffers
 
idle :: IORef GLfloat -> IORef GLfloat -> IdleCallback
idle angle delta = do
  d <- get delta
  angle $~! (+ d)
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
            concat [ [(fi x, fi (-n), 0.0), (fi x, fi n, 0.0)] | x <- [(-n) .. n]]
            ++
            concat [ [(fi (-n), fi y, 0.0), (fi n, fi y, 0.0)] | y <- [(-n) .. n]]
        fi = fromIntegral

