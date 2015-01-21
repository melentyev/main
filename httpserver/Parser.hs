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

mkTriple a b c = (a, b, c)
parseDigit = (read . (\c -> [c])) <$> digit
version = (,) <$> parseDigit <*> (char '.' >> parseDigit)

alpha = oneOf "abcdefghijklmnopqrstuvwxyz"
capAlpha = oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

(|-->) a b = a >>= (\e -> b >> return e)

urlEncodedChar = char '%' >> (string "00" <|> string "11")
urlEncoded = many $ (alpha <|> capAlpha <|> digit <|> urlEncodedChar)
queryParam = (,) <$> (urlEncoded |--> char '=') <*> urlEncoded
queryString = Map.fromList <$> many queryParam
method = fromString <$> many (noneOf " ")
path = spaces >> many (noneOf "? ")

requestLine :: GenParser Char st (Method, String, Map String String, (Int, Int))
requestLine = mkTriple 
	<$> method
	<*> path
	<*> option Map.empty (char '?' >> queryString)
	<*> (spaces >> string "HTTP/" >> version)

parseRequestLine :: BS.ByteString -> Maybe (Method, String, Map String String, (Int, Int))
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
	
fromEither :: Either _ b -> Maybe b
fromEither = either (const Nothing) (Just)