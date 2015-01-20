module Request (Request(..)) where

import Method
import qualified Data.Map as M

data Request = Request {
    path :: String,
    method :: Method,
    headers :: M.Map String String,
    queryParams :: M.Map String String
}