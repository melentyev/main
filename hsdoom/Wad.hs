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
	loadPnames,
	loadTextures,
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
import qualified Codec.Picture as C

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
	ldRightSidedef :: Maybe Sidedef, 
	ldLeftSidedef  :: Maybe Sidedef,
}

data Sidedef = Sidedef {
	sdXOffset    :: Int16,
	sdYOffser    :: Int16,
	sdTextureUp  :: String,
	sdTextureLow :: String,
	sdTextureMid :: String,
	sdSector     :: Sector
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

data Picture = {
	picWidth :: Int16,
	picHeight :: Int16,
	picLeftOff :: Int16,
	picRightOff :: Int16,
	picContents :: C.DynamicImage
}

data MapPatch = MapPatch {
	mpOriginx :: Int16,
	mpOriginy :: Int16,
	mpPatch   :: String
}

data Texture = Texture {
	txName    :: String,
	txWidth   :: Int16,
	txHeight  :: Int16,
	txPatches :: [MapPatch]
}

type Palette = [(Word8, Word8, Word8)]

linedefSize   = 14
sidedefSize   = 30
sectorSize    = 256
dirEntrySize  = 16
nameSize      = 8
vertexSize    = 4
palleteColors = 256
texMapPatsOff = 22
mapPatchSize  = 10


getInt16le = fromIntegral <$> getWord16le
getInt32le = fromIntegral <$> getWord32le

listToArray ::[a] -> UArray Int a
listToArray l = array (0, n-1) (zip [0 .. n-1] l) where n = length l

dropTailNulls :: String -> String
dropTailNulls = takeWhile ((/=) (chr 0))

readVertexes :: Int -> BL.ByteString -> [Vertex]
readVertexes 0 _ = []
readVertexes n s = runGet ((,) <$> getInt16le <*> getInt16le) s : readVertexes (n - 1) (BL.drop 4 s)

---------------------------------------
--------- LINEDEFS --------------------
---------------------------------------

readLinedef :: (Int -> Vertex) -> (Int -> Maybe Sidedef) -> BL.ByteString -> Linedef
readLinedef vs sds str = 
	Linedef { 
		ldStartVertex  = sv, 
		ldEndVertex    = ev, 
		ldFlags        = fromIntegral f, 
		ldSpecialType  = st, 
		ldSectorFlag   = sf, 
		ldRightSidedef = sds rsI, 
		ldLeftSidedef  = sds lsI 
	}
	where 
		sv = vs $ fromIntegral svI
		ev = vs $ fromIntegral evI
		[svI, evI, f, st, sf, rsI, lsI] = runGet p str
		p = sequence $ replicate 7 getInt16le

readLinedefs :: (Int -> Vertex) -> (Int -> Maybe Sidedef) -> Int -> BL.ByteString -> [Linedef]
readLinedefs _  sds 0 _   = []
readLinedefs vs sds n str = readLinedef vs str : readLinedefs vs (n - 1) (BL.drop linedefSize str)

---------------------------------------
--------- SIDEDEFS --------------------
---------------------------------------

readSidedef :: (Int -> Sector) -> BL.ByteString -> Sidedef
readSidedef sec str = 
	Sidedef {
		sdXOffset = xof,
		sdYOffser = yof,
		sdTextureUp  = dropTailNulls $ BL8.unpack tu,
		sdTextureLow = dropTailNulls $ BL8.unpack tl,
		sdTextureMid = dropTailNulls $ BL8.unpack tm,
		sdSector = sec $ fromIntegral s
	}
	where 
		(xof, yof, tu, tl, tm, s) = runGet parser str
		parser = ((,,,,,) <$> pi16 <*> pi16 <*> pn <*> pn <*> pn <*> pi16)
		pi16 = getInt16le
		pn   = getLazyByteString 8

readSidedefs :: (Int -> Sector) -> Int -> BL.ByteString -> [Sidedef]
readSidedefs _   0 _   = []
readSidedefs sec n str = readSidedef sec str : readSidedefs sec (n - 1) (BL.drop sidedefSize str)

---------------------------------------
--------- SECTORS ---------------------
---------------------------------------

readSector str = 
	Sector {
		secFloorHeight = fh,
		secCeilHeight  = ch,
		secFloorTex    = ft,
		secCeilTex     = ct,
		secLightLevel  = ll,
		secType        = tp,
		secTag         = tg
	}
	where 
		(fh, ch, ft, ct, ll, tp, tg) = runGet parser str
		parser = ((,,,,,,) <$> pi16 <*> pi16 <*> pn <*> pn <*> pi16 <*> pi16 <*> pi16)
		pi16 = getInt16le
		pn   = getLazyByteString 8

readSectors :: Int -> BL.ByteString -> [Sidedef]
readSectors 0 _   = []
readSectors n str = readSector str : readSectors (n - 1) (BL.drop sectorSize str)

---------------------------------------
--------- PNAMES ----------------------
---------------------------------------

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
	let de = fromJust $ List.find (\e -> deName e == "PNAMES") dir in 
	let off = deFilepos de in
	readPnames $ BL.drop (fromIntegral off) input

readDirectoryEntry :: BL.ByteString -> DirEntry
readDirectoryEntry s = 
	DirEntry { 
		deFilepos = fromIntegral fp, 
		deSize = fromIntegral sz, 
		deName = dropTailNulls $ BL8.unpack name
	}
	where 
		(fp, sz, name) = runGet parser s
		parser = (,,) <$> getInt32le <*> getInt32le <*> getLazyByteString 8

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

loadLevelGeometry :: Level -> BL.ByteString -> ([Vertex], [Linedef], [Sidedef], [Sector])
loadLevelGeometry level input = 
	(vs, lds, sds, scs)
	where
		locateLump fld size = 
			(fromIntegral $ (deSize . fld) level `div` (fromIntegral size), 
				fromIntegral $ (deFilepos . fld) level)
		getSidedef i = if i == -1 then Nothing else Just $ sds !! i
 		vs    = (uncarry . readVertexes) (locateLump levVertexes vertexSize) in
 	    scs   = (uncarry . readSectors) (locateLump levSectors sectorSize) in
 	    sds   = (uncarry . readSidedefs (\i -> scs !! i) ) (locateLump levLinedefs linedefSize)
 	    lds   = (uncarry . readLinedefs (\i -> vs !! i) (getSidedef) (locateLump levLinedefs linedefSize)

readMapPatch :: (Int -> String) -> BL.ByteString -> MapPatch
readMapPatch pns str = 
	MapPatch {
		mpOriginx = ox,
		mpOriginy = oy,
		mpPatch   = pns mp
	}
	where [ox, mp, oy] = runGet (sequence $ replicate 3 getInt16le) str

loadMapPatches :: Int -> (Int -> String)-> BL.ByteString -> [MapPatch]
loadMapPatches 0 _    _   = []
loadMapPatches n pns str = readMapPatch str : loadMapPatches pns (n - 1) (BL.drop mapPatchSize str)

loadTextureMap :: (Int -> String) -> BL.ByteString -> Texture
loadTextureMap pns s = Texture {
		txName    = dropTailNulls $ BL8.unpack nm,
		txWidth   = w,
		txHeight  = h,
		txPatches = loadMapPatches (fromIntegral pc) pns rest
	}
	where
	rest = BL.drop texMapPatsOff s
	(nm, _, w, h, _, pc) = runGet p s
	p = (,,,,,) <$> getLazyByteString 8 <*> getInt32le 
			<*> getInt16le <*> getInt16le 
			<*> getInt32le <*> getInt16le

loadTextures :: [DirEntry] -> BL.ByteString -> [Texture]
loadTextures dir input = 
	concat $ map loadtext ["TEXTURE1", "TEXTURE2"]
	where 
		pnames = loadPnames dir input
		pns i = pnames !! i
		loadtext lumpname = loadTexturesFromLump $ loadLump dir lumpname input
			--let de = fromJust $ List.find (\e -> deName e == lumpname) dir in 
			--let off = deFilepos de in
			-- $ BL.drop (fromIntegral off) input
		loadTexturesFromLump str = 
		    let cnt = runGet getInt32le str in
		    let offsets = runGet (sequence $ replicate (fromIntegral cnt) getInt32le) $ BL.drop 4 str in
		    map (\off -> loadTextureMap $ BL.drop off str) offsets
	

loadPicture :: BL.ByteString -> Picture
loadPicture lump = 
	Picture {
		picWidth = pw,
		picHeight = ph, 
		picLeftOff = lo,
		picRightOff = ro,
		picContents = picdata
	}
	where
		[pw, ph, lo, ro] = runGet (sequence $ replicate 4 getInt16le) lump
		coloff  = runGet (sequence $ replicate pw getInt32le) $ BL.drop 8 lump
		cols    = map readcol coloff
		readcol off = readposts $ BL.drop (fromIntegral off) lump
		readposts s = let (piccoloff, cnt) = runGet ((,) <$> getWord8 <*> getWord8)

{-loadTextureImage :: Texture -> [DirEntry] -> BL.ByteString -> C.DynamicImage
loadTextureImage t dir input = 
	generateImage
	where 
		pics = map loadPicture txPatches t -}
	

writeFlatPng path pal flatData = 
	writePng path $ generateImage pixelRenderer 64 64
    where 
   		pixelRenderer x y = PixelRGB8 r g b 
   			where
   					(r, g, b)  = pal !! getIndex (y * 64 + x)
   		getIndex i        = fromIntegral $ runGet getWord8 $ BL.drop (fromIntegral i) flatData

getLevels :: [DirEntry] -> UArray Int Level
getLevels des = listToArray levels 
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

loadLump :: [DirEntry] -> String -> BL.ByteString -> BL.ByteString
loadLump dir lumpname input = 
	let de  = fromJust $ List.find (\e -> deName e == lumpname) dir in 
	let off = deFilepos de in
	let sz  = deSize de in
	BL.take (fromIntegral de) $ BL.drop (fromIntegral off) input

loadDirectory :: BL.ByteString -> IO [DirEntry]
loadDirectory input = do
	let iwadSig = BL8.pack "IWAD"
	if not $ BL.take 4 input == iwadSig then error "Not IWAD"
	else do 
		let (nlumps, dirpos) = join (***) fromIntegral $ runGet ((,) <$> getWord32le <*> getWord32le) (BL.drop 4 input)
		let slicedPref   = BL.drop (fromIntegral dirpos) input
		let sliced       = BL.take (fromIntegral (nlumps * dirEntrySize) ) slicedPref
		M.fromList $ map (\de -> (deName de, de)) readDirectory (fromIntegral nlumps) sliced