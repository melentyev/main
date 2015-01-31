module Draw (
	drawMap
	) where

import Graphics.UI.GLUT

import qualified Wad as W
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.IORef

type DrawScene = IORef (IO () )

color3f r g b  = color $ Color3 r g (b :: GLfloat)
vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)

drawMap :: W.Level -> BL.ByteString -> IO ()
drawMap level input = do
    --print level
    let (vs, lds) = W.loadLevelGeometry level input
    let (x1, y1) = (minimum (map fst vs), minimum (map snd vs))
    let (x2, y2) = (maximum (map fst vs), maximum (map snd vs))
    print (x1, y1)
    print (x2, y2)
    drawScene <- newIORef $ drawMap' vs lds
    print $ take 10 vs
    initControl drawScene
    mainLoop

gameLoop level input = do
    --let (vs, lds, sects) = W.loadLevelGeometry level input
    return ()
    


drawMap' :: [W.Vertex] -> [W.Linedef] -> IO ()
drawMap' vs lds = do
    let (x1, y1) = (minimum (map fst vs), minimum (map snd vs))
    let (x2, y2) = (maximum (map fst vs), maximum (map snd vs))
    let maxW = (fromIntegral (x2 - x1) :: GLfloat)
    let maxH = (fromIntegral (y2 - y1) :: GLfloat)
    let trVert (x, y) = Vertex3 (fromIntegral (x - x1) / maxW) (fromIntegral (y - y1) / maxH) 0
    let levlines = concat $ map (\l -> [trVert $ W.ldStartVertex l, trVert $ W.ldEndVertex l]) lds
    color3f 1.0 1.0 0
    renderPrimitive Lines $
        mapM_ (\v -> vertex v) levlines

initControl :: DrawScene -> IO ()
initControl drawScene = do
    (pgNm, args)    <- getArgsAndInitialize
    --initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    window          <- createWindow "Hello World"
    displayCallback $= display drawScene
    reshapeCallback $= Just reshape
    --depthFunc       $= Just Less
    --clearColor      $= (Color4 0.0 0.0 0 0.0)
    windowSize      $= Size 480 480
    --angle <- newIORef 0
    --delta <- newIORef 0.1
    --pos   <- newIORef (0, 0)
    --keyboardMouseCallback $= Just (keyboardMouse delta pos)
    --idleCallback $= Just (idle angle delta)
    
 
myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [ (sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12] ]

reshape :: ReshapeCallback
reshape size = do
    viewport $= (Position 0 0, size)
    postRedisplay Nothing

display :: DrawScene -> DisplayCallback
display drawScene = do
    clear [ColorBuffer]
    ds <- get drawScene
    ds
    --color3f 1.0 0 0
    --renderPrimitive Lines $
    --    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
    flush