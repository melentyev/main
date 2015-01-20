module Resource (
	Resource(..), 
	path, 
	method, 
	callback) where

import Method 
import qualified Request as Req
import qualified Response as Resp

data Resource = Resource { 
	path :: String,
	method :: Method,
	callback :: Req.Request -> Resp.Response
}

instance Show Resource where
	show r = "Resource {path=" ++ show (path r) ++", method=" ++ show (method r) ++ "}"