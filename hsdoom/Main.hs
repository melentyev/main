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
--import qualified Text.Show.Pretty as Pr

imageCreator :: String -> IO ()
imageCreator path = writePng path $ generateImage pixelRenderer 250 300
   where pixelRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128

runGame :: IO () 
runGame = do
    lazyWad   <- openBinaryFile "doom.wad" ReadMode >>= BL.hGetContents
    dir       <- W.loadDirectory lazyWad
    pnames    <- loadPnames dir lazyWad
    let levels = W.getLevels dir
    D.drawMap (levels !! 0) lazyWad

main :: IO ()
main = runGame

