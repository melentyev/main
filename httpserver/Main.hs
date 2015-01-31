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
import DataDecl
import System.IO
import qualified Data.List as List

restService = S.restService 1488 resources
service = S.service 1488 webSiteHandler

resources = 
	[ 
		Res.Resource { Res.path = "/testget", Res.method = Method.Get, Res.callback = testget }
	]

docRoot = "C:\\Users\\user\\wwwdata\\yii1116\\demos\\blog" 

phpHandler :: Req.Request -> String -> String -> BodyStream -> SendResponse -> IO BS.ByteString 
phpHandler req url query_str body sendResp = do
	let startInfo = (proc "C:\\Users\\user\\PHP\\php-cgi.exe" []) { 
		std_out = CreatePipe,
		std_in = CreatePipe,
		env = 
			Just [
				--("SCRIPT_FILENAME", "C:\\Users\\user\\wwwdata/index.php"),
				--("DOCUMENT_ROOT", "C:\\Users\\user\\wwwdata"),
				("REQUEST_METHOD", (show . Req.method) req),
				("DOCUMENT_ROOT", docRoot),
				("SCRIPT_FILENAME", docRoot ++ "\\index.php"),
				("REQUEST_URI", url),
				("SCRIPT_NAME", "/index.php"),
				("SCRIPT_FILENAME", docRoot ++ "\\index.php"),
				("QUERY_STRING", query_str)] 
	}
	bodyData <- case Req.method req of 
					Method.Get -> return BS.empty
					_          -> fst <$> body (Req.contentLength req)
	(Just hin, Just hout, _, _) <- createProcess startInfo
	hPutStr $ B8.unpack body
	cont <- hGetContents hout
	return $ B8.pack cont

staticHandler :: String -> BodyStream -> SendResponse -> IO BS.ByteString 
staticHandler url body sendResp = do
	let path = docRoot ++ (map (\c -> if c == '/' then '\\' else c) url)
	h <- openBinaryFile path ReadMode
	cont <- hGetContents h
	return $ B8.pack cont
	--return BS.empty


webSiteHandler :: Req.Request -> BodyStream -> SendResponse -> IO ()
webSiteHandler req body sendResp = do
	let (path, query) = span ((/=)'?') (Req.url req)
	cont <- 
		if ".php" `elem` (List.tails path) then 
			do
				let query_str = drop 1 query
				print "(path, query_str)"
				print $ path
				print $ query_str
				phpHandler req (Req.url req) query_str body sendResp
		else 
			do
				staticHandler path body sendResp
	--sendResp $ B8.append (B8.pack ("HTTP/1.1 200 OK\r\n") ) cont
	sendResp $ B8.append (B8.pack ("HTTP/1.1 200 OK\r\n") ) cont

testget req _ = return $ Resp.Response { 
	Resp.httpVersion = (1, 1), 
	Resp.statusCode = StatusCode.Ok, 
	Resp.statusMessage = "OK", 
	Resp.headers = Map.empty, 
	Resp.body = BS.empty
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
			--("SCRIPT_FILENAME", "C:\\Users\\user\\wwwdata/index.php"),
			--("PHP_SELF", "/index.php"),
			--("DOCUMENT_ROOT", "C:\\Users\\user\\wwwdata"),
			("REQUEST_METHOD", "GET"),
			("DOCUMENT_ROOT", "C:\\Users\\user\\wwwdata\\yii1116\\demos\\blog"),
			("SCRIPT_FILENAME", "C:\\Users\\user\\wwwdata\\yii1116\\demos\\blog\\index.php"),
			("REQUEST_URI", "/index.php?r=site/login"),
			("SCRIPT_NAME", "/index.php"),
			("SCRIPT_FILENAME", "/index.php"),
			("QUERY_STRING", "r=site/login")

		] 
	}
	(Just hin, Just hout, _, _) <- createProcess startInfo
	cont <- hGetContents hout
	putStrLn cont
	-- ""
	-- SCRIPT_FILENAME = "C:\Users\user\wwwdata\index.php"

runServer :: IO ()
runServer = S.start service

main :: IO()
main = runServer