module Parser (
	parseRequestLine, 
	parseHeader
) where 

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Control.Applicative ((<*>), (<$>))
import Method
import qualified Data.Map as Map

parseDigit = ( (read  :: String -> Int) . (\c -> [c])) <$> digit
version = (,) <$> parseDigit <*> (char '.' >> parseDigit)

alpha = oneOf "abcdefghijklmnopqrstuvwxyz"
capAlpha = oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

(|-->) a b = a >>= (\e -> b >> return e)

uenc c = try (string (uenc' c) >> return c) where 
	uenc' ('\\') = "5C"
	uenc' ('/')  = "2F"

urlEncodedChar = char '%' >> ( uenc '\\' <|> uenc '/' )
urlEncoded = many $ (alpha <|> capAlpha <|> digit <|> char '-' <|> urlEncodedChar)
queryParam = (,) <$> (urlEncoded |--> char '=') <*> urlEncoded
queryString = Map.fromList <$> many queryParam
method = fromString <$> many (noneOf " ")
path = spaces >> many (noneOf "? ")

requestLine :: GenParser Char st (Method, String, Map.Map String String, (Int, Int))
requestLine = (,,,) 
	<$> method
	<*> path
	<*> option Map.empty (char '?' >> queryString)
	<*> (spaces >> string "HTTP/" >> version)

parseRequestLine :: BS.ByteString -> Maybe (Method, String, Map.Map String String, (Int, Int))
parseRequestLine line =
    fromEither $ parse requestLine "(unknown)" (C8.unpack line)

header = (,) <$> many (noneOf ": ") <*> (char ':' >> char ' ' >> many anyChar)

parseHeader :: BS.ByteString -> Maybe (String, String, Bool)
parseHeader line = 
	if BS.null line 
	then Just ("", "", True)
	else case fromEither $ parse header "(unknown)" (C8.unpack line) of 
		Just (k, v) -> Just (k, v, False)
		_ -> Nothing
	
fromEither :: Either a b -> Maybe b
fromEither = either (const Nothing) (Just)