module Main where

import System.IO
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Binary.Get
import Data.Word
import Data.Int
import Control.Applicative ((<*>), (<$>))
import Control.Arrow ((***))
import Control.Monad (join, (>=>))
import Numeric (showHex)
import Codec.Picture
import qualified Wad as W
import qualified Draw as D
import qualified Draw3 as D3
--import qualified Text.Show.Pretty as Pr

imageCreator :: String -> IO ()
imageCreator path = writePng path $ generateImage pixelRenderer 250 300
   where pixelRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128

runGame :: IO () 
runGame = do
    lazyWad   <- openBinaryFile "doom.wad" ReadMode >>= BL.hGetContents
    dir       <- W.loadDirectory lazyWad
    let levels = W.getLevels dir
    let texts  = W.loadTextures dir lazyWad
    let name = W.tpPatch (W.txPatches (texts !! 0) !! 0)
    --W.loadPicture $ W.loadLump dir name lazyWad
    --D.drawMap (levels !! 0) lazyWad
    runLevel (levels ! 0) lazyWad

main :: IO ()
main = d3dmain

{-let playPalLump  = fromJust $ M.lookup "PLAYPAL" dm
        --let ld           = readLinedefs (fromIntegral ldsCnt) (BL.drop ldsOffset input)


        {-let p1           = readPallet 0 $ BL.drop (fromIntegral $ deFilepos playPalLump) input
        mapM_ (\(i, f)-> 
                writeFlatPng 
                    ("C:\\Users\\user\\flats\\img" ++ show i ++ ".png") 
                    p1 
                    (BL.drop (fromIntegral $ deFilepos f) input)
            ) [(i, fromJust $ M.lookup (flats !! i) dm) | i <- [0 .. length flats - 1]]-}

