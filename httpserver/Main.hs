module Main where

import qualified Service as S
import qualified Method as Method
import qualified StatusCode as StatusCode
import qualified Request as Req
import qualified Response as Resp
import qualified Resource as Res
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Control.Applicative ((<*>), (<$>))
import System.Process
import System.IO 
import System.Directory

service = S.Service { S.port = 1488, S.resources = resources }

resources = 
	[ 
		Res.Resource { Res.path = "/testget", Res.method = Method.Get, Res.callback = testget }
	]

nullstr = (B8.pack "")

testget req _ = return $ Resp.Response { 
	Resp.httpVersion = (1, 1), 
	Resp.statusCode = StatusCode.Ok, 
	Resp.statusMessage = "OK", 
	Resp.headers = Map.empty, 
	Resp.body = nullstr
}

mapStream :: (BS.ByteString -> a) -> Res.BodyStream -> IO([a])
mapStream f stream =
    stream 10 >>= (\acceptedData -> 
									(case acceptedData of 
										Res.Error                 -> return []
										Res.Success (bs, stream') -> (:) <$> return (f bs) <*> mapStream f stream'
										Res.EOF 				  -> return []
										 ))

{-foreachStream :: IO ((BS.ByteString -> IO ()) -> Res.BodyStream -> ())
foreachStream f stream = do
    acceptedData <- stream 10x
	(case acceptedData of 
			Res.Success (bs, stream') -> f bs >> foreachStream f stream'
			Res.EOF 				  -> return ()
			Res.Error                 -> return () )-}

getfile req bodyStream = do
	res <- mapStream (id) bodyStream
	print $ res
	return $ Resp.Response {
		Resp.httpVersion = (1, 1), 
		Resp.statusCode = StatusCode.Ok, 
		Resp.statusMessage = "OK", 
		Resp.headers = Map.empty, 
		Resp.body = B8.pack "Hello, World"
	}

runPhp :: IO ()
runPhp = do
	setCurrentDirectory "C:\\Users\\user\\PHP\\"
	cdir <- getCurrentDirectory
	print cdir
	let startInfo = (proc "php-cgi.exe" []) { 
		std_out = CreatePipe, 
		std_in = CreatePipe, 
		env = Just [
			("SCRIPT_FILENAME", "C:\\Users\\user\\wwwdata/index.php"),
			--("PHP_SELF", "/index.php"),
			("DOCUMENT_ROOT", "C:\\Users\\user\\wwwdata"),
			("REQUEST_METHOD", "GET")
		] 
	}
	(Just hin, Just hout, _, _) <- createProcess startInfo
	cont <- hGetContents hout
	putStrLn cont
	-- ""
	-- SCRIPT_FILENAME = "C:\Users\user\wwwdata\index.php"

main :: IO()
main = runPhp --S.start service