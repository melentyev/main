module Resource (
	Resource(..),
	handleRequest) where

import Method 
import qualified Request as Req
import qualified Response as Resp

data Resource = Resource { 
	path :: String,
	method :: Method,
	callback :: Req.Request -> IO Resp.Response
}

instance Show Resource where
	show r = "Resource {path=" ++ show (path r) ++", method=" ++ show (method r) ++ "}"


handleRequest :: Resource -> Req.Request -> IO Resp.Response
handleRequest = callback