module Wad (
	DirEntry(..),
	Level(..),
	Thing(..),
	Linedef(..),
	Sidedef(..),
    Seg(..),
	Vertex,
	Sector(..),
	Subsector(..),
	Palette,
	loadDirectory,
	getLevels,
	loadLevelGeometry,
	) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Binary.Get
import Data.Word
import Data.Int
import Control.Applicative ((<*>), (<$>))
import Control.Arrow ((***))
import Control.Monad (join)
import Numeric (showHex)
import Data.Char (chr)
import qualified Data.List as List
import Data.Maybe (fromJust, isJust)
import qualified Data.Map as M
import Codec.Picture (writePng, PixelRGB8 (..), generateImage)

type Vertex = (Int16, Int16)

data DirEntry = DirEntry {
	deFilepos :: Int32,
	deSize    :: Int32,
	deName    :: String
} deriving (Show)

data Level = Level {
	levName       :: String,
	levThings     :: DirEntry,
	levLinedefs   :: DirEntry,
	levSidedefs   :: DirEntry,
	levVertexes   :: DirEntry,
	levSegs       :: DirEntry,
	levSubsectors :: DirEntry,
	levNodes      :: DirEntry,
	levSectors    :: DirEntry,
	levReject     :: DirEntry,
	levBlockmap   :: DirEntry
} deriving (Show)

data Linedef = Linedef {
	ldStartVertex  :: Vertex,  
	ldEndVertex    :: Vertex,
	ldFlags        :: Int16,
	ldSpecialType  :: Int16,
	ldSectorFlag   :: Int16,
	ldRightSidedef :: Int16, 
	ldLeftSidedef  :: Int16
}

data Sidedef = Sidedef {
	sdXOffset    :: Int16,
	sdYOffser    :: Int16,
	sdTextureUp  :: String,
	sdTextureLow :: String,
	sdTextureMid :: String,
	sdSector     :: Int16
}

data Seg = Seg {
	segStartVertex :: Vertex,
	segEndVertex   :: Vertex,
	segAngle       :: Int16,
	segLinedef     :: Int16,
	segDirection   :: Int16,
	segOffset      :: Int16
}

data Sector = Sector {
	secFloorHeight :: Int16,
	secCeilHeight  :: Int16,
	secFloorTex    :: String,
	secCeilTex     :: String,
	secLightLevel  :: Int16,
	secType        :: Int16,
	secTag         :: Int16
}

data Subsector = Subsector {
	ssCount       :: Int16,
	ssFirstNumber :: Int16
}

data Thing = Thing {
	tXPos  :: Int16,
	tYPos  :: Int16,
	tAngle :: Int16,
	tTType :: Int16,
	tFlags :: Int16
}

data MapPatch = MapPatch {
	mpOriginx :: Int16,
	mpOriginy :: Int16,
	mpPatch   :: Int16
}

data Texture = Texture {
	txName    :: String,
	txWidth   :: Int16,
	txHeight  :: Int16,
	txPatches :: [MapPatch]
}

type Palette = [(Word8, Word8, Word8)]

linedefSize   = 14
dirEntrySize  = 16
nameSize      = 8
vertexSize    = 4
palleteColors = 256
texMapPatsOff = 22
mapPatchSize  = 10

getInt16le = fromIntegral <$> getWord16le
getInt32le = fromIntegral <$> getWord32le

dropTailNulls :: String -> String
dropTailNulls = takeWhile ((/=) (chr 0))

readVertexes :: Int -> BL.ByteString -> [Vertex]
readVertexes 0 _ = []
readVertexes n s = 
	runGet ((,) <$> getInt16le <*> getInt16le) s
		: readVertexes (n - 1) (BL.drop 4 s)

nthVertex i = (0, 0)--vertexList !! i

readLinedef :: (Int -> Vertex) -> BL.ByteString -> Linedef
readLinedef vs str = 
	Linedef { 
		ldStartVertex  = sv, 
		ldEndVertex    = ev, 
		ldFlags        = fromIntegral f, 
		ldSpecialType  = st, 
		ldSectorFlag   = sf, 
		ldRightSidedef = rs, 
		ldLeftSidedef  = ls 
	}
	where 
		sv = vs $ fromIntegral svI
		ev = vs $ fromIntegral evI
		[svI, evI, f, st, sf, rs, ls] = runGet p str
		p = sequence $ replicate 7 getInt16le
		--p = (,,,,,,) <$> getInt16le <*> getInt16le <*> getInt16le <*> getInt16le <*> getInt16le <*> getInt16le <*> getInt16le

readLinedefs :: Int -> (Int -> Vertex) -> BL.ByteString -> [Linedef]
readLinedefs 0 _  _   = []
readLinedefs n vs str = readLinedef vs str : readLinedefs (n - 1) vs (BL.drop linedefSize str)

readPnames :: BL.ByteString -> [String]
readPnames input = 
	readPnames' cnt rest
	where 
		readPnames' 0 _   = []
		readPnames' n str = readPname str : readPnames' (n - 1) (BL.drop nameSize str)
		readPname  = dropTailNulls . BL8.unpack . BL.take nameSize
		cnt        = runGet getInt32le input
		rest       = BL.drop 4 input

loadPnames :: [DirEntry] -> BL.ByteString -> [String]
loadPnames dir input = 
	let de = List.find (\e -> deName e == "PNAMES") dir in 
	let off = deFilepos de in
	readPnames $ BL.drop off input

readDirectoryEntry :: BL.ByteString -> DirEntry
readDirectoryEntry s = 
	DirEntry { 
		deFilepos = fromIntegral fp, 
		deSize = fromIntegral sz, 
		deName = name'
	}
	where 
		name' =  dropTailNulls $ BL8.unpack name
		(fp, sz, name) = runGet parser s
		parser = (,,) <$> getWord32le <*> getWord32le <*> getLazyByteString 8

readDirectory :: Int -> BL.ByteString -> [DirEntry]
readDirectory 0 _     = []
readDirectory n input = readDirectoryEntry input : readDirectory (n - 1) (BL.drop dirEntrySize input)

getFlats :: [DirEntry] -> [DirEntry]
getFlats = filter (\e -> deSize e /= 0) 
		 . takeWhile (\e -> deName e /= "F_END") 
		 . dropWhile (\e -> deName e /="F_START") 

readPallet :: Int -> BL.ByteString -> Palette
readPallet n =  parse . BL.drop (768 * fromIntegral n)
	where parse = runGet (sequence $ replicate 256 ((,,) <$> getWord8 <*> getWord8 <*> getWord8))

loadLevelGeometry :: Level -> BL.ByteString -> ([Vertex], [Linedef])
loadLevelGeometry level input = 
	let vcnt  = fromIntegral $ (deSize . levVertexes) level `div` vertexSize in
	let voff  = fromIntegral $ (deFilepos . levVertexes) level in
	let vdat  = BL.drop voff input in 
 	let vs    = readVertexes vcnt vdat in
 	let ldcnt = fromIntegral $ (deSize . levLinedefs) level `div` (fromIntegral linedefSize) in
 	let ldoff = fromIntegral $ (deFilepos . levLinedefs) level in
 	let lddat = BL.drop ldoff input in 
 	let lds   = readLinedefs ldcnt (\i -> vs !! i) lddat in 
 	(vs, lds)

readMapPatch = str = 
	MapPatch {
		mpOriginx = ox,
		mpOriginy = oy,
		mpPatch   = mp
	}
	where [ox, mp, oy] = runGet (sequence . replicate 3 getInt16le) str

loadMapPatches :: Int -> BL.ByteString -> [MapPatch]
loadMapPatches 0 _   = []
loadMapPatches n str = readMapPatch str : loadMapPatches (n - 1) (BL.drop mapPatchSize str)

loadTextureMap :: BL.ByteString -> Texture
loadTextureMap s = Texture {
		txName    = dropTailNulls nm,
		txWidth   = w,
		txHeight  = h,
		txPatches = loadMapPatches pc rest
	}
	where
	rest = BL.drop texMapPatsOff s
	(nm, _, w, h, _, pc) = runGet p s
	p = (,,,,,) <$> getLazyByteString 8 <*> getInt32le 
			<*> getInt16le <*> getInt16le 
			<*> getInt32le <*> getInt16le

loadTextures :: BL.ByteString -> [Texture]
loadTextures input = 
    let cnt = runGet getWord32le input in
    let offsets = runGet (sequence $ replicate cnt getInt32le) $ BL.drop 4 input in
    map (\off -> loadTextureMap $ BL.drop off input) offsets
    


writeFlatPng path pal flatData = 
	writePng path $ generateImage pixelRenderer 64 64
    where 
   		pixelRenderer x y = PixelRGB8 r g b 
   			where
   					(r, g, b)  = pal !! getIndex (y * 64 + x)
   		getIndex i        = fromIntegral $ runGet getWord8 $ BL.drop (fromIntegral i) flatData

getLevels :: [DirEntry] -> [Level]
getLevels des = levels
	where 
	levels      = map (initLevel . fromJust) nonEmpty
	nonEmpty    = filter isJust $ map findLevel levNames
	initLevel (nm : t : ld : sd : v : sg : ss : n : sc : rej : blk : _) = 
		Level {
			levName       = deName nm, 
			levThings     = t,
			levLinedefs   = ld,
			levSidedefs   = sd,
			levVertexes   = v,
			levSegs       = sg,
			levSubsectors = ss,
			levNodes      = n,
			levSectors    = sc,
			levReject     = rej,
			levBlockmap   = blk
		}
	findLevel s = case span (\e -> deName e /= s) des of 
						(_, [])     -> Nothing
						(_, levdat) -> Just levdat
	levNames    = ["E" ++ show x ++ "M" ++ show  y | x <- [1 .. 9], y <- [1 .. 9]]

loadDirectory :: BL.ByteString -> IO [DirEntry]
loadDirectory input = do
	let iwadSig = BL8.pack "IWAD"
	if not $ BL.take 4 input == iwadSig then error "Not IWAD"
	else do 
		let (nlumps, dirpos) = join (***) fromIntegral $ runGet ((,) <$> getWord32le <*> getWord32le) (BL.drop 4 input)
		--print $ showHex nlumps ""
		--print $ showHex dirpos ""
		let slicedPref   = BL.drop (fromIntegral dirpos) input
		let sliced       = BL.take (fromIntegral (nlumps * dirEntrySize) ) slicedPref
		let dir          = readDirectory (fromIntegral nlumps) sliced
		return dir
		--let flats        = getFlats
		--let levels       = getLevels dir
		--let dm           = M.fromList $ map (\de -> (deName de, de)) dir
		--let ldsLump      = fromJust $ M.lookup "LINEDEFS" dm
		--let ldsSize      = deSize $ ldsLump
		--let ldsCnt       = ldsSize `div` fromIntegral dirEntrySize
		--let ldsOffset    = fromIntegral $ deFilepos ldsLump
		--pn              <- readPnames $ M.lookup "PNAMES" dm

		--let f1Lump       = fromJust $ M.lookup (flats !! 0) dm
		--let playPalLump  = fromJust $ M.lookup "PLAYPAL" dm
		--let ld           = readLinedefs (fromIntegral ldsCnt) (BL.drop ldsOffset input)


		{-let p1           = readPallet 0 $ BL.drop (fromIntegral $ deFilepos playPalLump) input
		mapM_ (\(i, f)-> 
				writeFlatPng 
					("C:\\Users\\user\\flats\\img" ++ show i ++ ".png") 
					p1 
					(BL.drop (fromIntegral $ deFilepos f) input)
			) [(i, fromJust $ M.lookup (flats !! i) dm) | i <- [0 .. length flats - 1]]
		
		print playPalLump-}		
		