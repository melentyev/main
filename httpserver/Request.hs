module Request (
	Request(..),
	contentLength) where

import Method
import qualified Data.Map as M
import qualified Data.Maybe as Mb

data Request = Request {
	httpVersion :: (Int, Int),
	url :: String,
    path :: String,
    method :: Method,
    headers :: M.Map String String,
    queryParams :: M.Map String String
} deriving (Show)

contentLength :: Request -> Int
contentLength = read . Mb.fromJust . (M.lookup "Content-Length") . headers

