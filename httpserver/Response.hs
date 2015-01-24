module Response (
	Response(..),
	badRequest,
	notFound,
	setBody,
	rawData
	) where

import qualified Data.Map as M
import StatusCode
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

data Response = Response { 
	httpVersion :: (Int, Int),
	statusCode :: StatusCode,
	statusMessage :: String,
	headers :: M.Map String String,
	body :: BS.ByteString
} deriving (Show)

nullstr = (B8.pack "")

defaultResponse = (Response {
	httpVersion = (1, 1),
	statusCode = Ok,
	statusMessage = "",
	headers = M.empty,
	body = nullstr
})

notFound = setStatusCode (setStatusMessage defaultResponse "Not found") NotFound
badRequest = setStatusCode (setStatusMessage defaultResponse "Bad request") BadRequest

setStatusMessage r m = r { statusMessage = m }
setStatusCode r c = r { statusCode = c }
setBody r b = r { body = b, headers = M.insert "Content-Length" (show $ BS.length b) (headers r) }

rawData rawResponse =
	B8.pack $ "HTTP/" ++ show v1 ++ "." ++ show v2 
		++ " " ++ (show . statusCode) response ++ " "
		++ statusMessage response ++ "\r\n" ++ headersRaw ++ "\r\n" ++ (B8.unpack . body) response
	where 
		response = setBody rawResponse $ body rawResponse
		(v1, v2) = httpVersion response
		headerPairs = M.assocs (headers response)
		headerLines = map (\(a, b) -> a ++ ": " ++ b ++ "\r\n") headerPairs
		headersRaw = foldr (++) "" headerLines 

