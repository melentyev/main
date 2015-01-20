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
testget req = return $ Resp.Response { 
	Resp.httpVersion = (1, 1), 
	Resp.statusCode = StatusCode.Ok, 
	Resp.statusMessage = "OK", 
	Resp.headers = Map.empty, 
	Resp.body = ""
}


main :: IO()
main =
    S.start service