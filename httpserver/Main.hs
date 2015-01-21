module Main where

import qualified Service as S
import qualified Method as Method
import qualified StatusCode as StatusCode
import qualified Request as Req
import qualified Response as Resp
import qualified Resource as Res
import qualified Data.Map as Map
service = S.Service { S.port = 1488, S.resources = resources }

resources = 
	[ 
		Res.Resource { Res.path = "/testget", Res.method = Method.Get, Res.callback = testget }
	]

testget req _ = return $ Resp.Response { 
	Resp.httpVersion = (1, 1), 
	Resp.statusCode = StatusCode.Ok, 
	Resp.statusMessage = "OK", 
	Resp.headers = Map.empty, 
	Resp.body = ""
}

mapStream :: IO ((ByteString -> IO ()) -> Res.BodyStream -> ())
mapStream f stream = do
    acceptedData <- bodyStream 10
	case acceptedData of 
		Success (bs, stream') -> f bs >> mapStream f stream'
		EOF 				  -> return ()
		Error                 -> return () 

getfile req bodyStream = do
	res <- mapStream (print) bodyStream
	return $ Resp.Response {
		Resp.httpVersion = (1, 1), 
		Resp.statusCode = StatusCode.Ok, 
		Resp.statusMessage = "OK", 
		Resp.headers = Map.empty, 
		Resp.body = show res
	}


main :: IO()
main =
    S.start service