module Response (
	Response(..), 
	httpVersion, 
	statusCode, 
	statusMessage, 
	headers, 
	body) where

import qualified Data.Map as M
import StatusCode

data Response = Response { 
	httpVersion :: (Int, Int),
	statusCode :: StatusCode,
	statusMessage:: String,
	headers :: M.Map String String,
	body :: String
}

responseRawData response =
	"HTTP/" ++ show v1 ++ "." ++ show v2 
		++ " " ++ (show . statusCode) response ++ " "
		++ statusMessage response ++ "\r\n" ++ headersRaw ++ "\r\n" ++ body response
	where 
		(v1, v2) = httpVersion response
		headerPairs = M.assocs (headers response)
		headerLines = map (\(a, b) -> a ++ b) headerPairs
		headersRaw = foldr (++) "" headerLines 