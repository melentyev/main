module Resource (
	Resource(..),
	StreamResult(..),
	BodyStream,
	handleRequest) where

import Method 
import qualified Request as Req
import qualified Response as Resp
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as BS
import DataDecl


data Resource = Resource { 
	path :: String,
	method :: Method,
	callback :: Req.Request -> BodyStream -> IO Resp.Response
}

instance Show Resource where
	show r = "Resource {path=" ++ show (path r) ++", method=" ++ show (method r) ++ "}"


handleRequest :: Resource -> Req.Request -> BodyStream -> IO Resp.Response
handleRequest = callback