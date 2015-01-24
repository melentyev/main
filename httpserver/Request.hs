module Request (Request(..)) where

import Method
import qualified Data.Map as M

data Request = Request {
	httpVersion :: (Int, Int),
    path :: String,
    method :: Method,
    headers :: M.Map String String,
    queryParams :: M.Map String String
} deriving (Show)


