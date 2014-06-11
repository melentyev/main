module Main where

import Data.Char
import Data.List
-- import qualified Data.Text as T
 
data JValue = JString String     
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show) 

renderJValue :: JValue -> String 
renderJValue (JString s) = show s 
renderJValue (JNumber n) = show n 
renderJValue (JBool b) = show b
renderJValue x = show x

ltrim :: String -> String
ltrim = dropWhile (== ' ') 

rtrim :: String -> String
rtrim = dropWhileEnd (== ' ') 

stateLTrimmed :: (a, String) -> (a, String)
stateLTrimmed (a, xs) = (a, ltrim xs)

trim :: String -> String
trim = ltrim . rtrim

parseJValue :: String -> (JValue, String)
parseJValue (' ' : xs) = parseJValue xs
parseJValue ('{' : xs) = (JObject o, xs') where (o, xs') = (parseJObject [] xs)
parseJValue xs 
    | "true" `isPrefixOf` xs = (JBool True, drop 4 xs)
    | "false" `isPrefixOf` xs = (JBool False, drop 5 xs)
parseJValue ('\"' : xs) = (JString s, xs') where (s, xs') = parseJString "" xs
parseJValue (xs)        = 
    let isNumberPart c = c /= '.' && not (isDigit c)
        (val, xs') = break isNumberPart xs
        (val', _) = head $ (reads val :: [(Double, String)]) 
    in
        (JNumber val', xs')


parseJObject :: [(String, JValue)] -> String -> ([(String, JValue)], String)
parseJObject acc (' ' : xs)     = parseJObject acc xs
parseJObject acc ('}' : xs)     = (reverse acc, xs)
parseJObject acc ('\"' : xs)    = 
    let (key, restAfterKey) = stateLTrimmed $ parseJString "" xs
        parseJObjectValue (':' : afterColon) =
                let (val, rest) = stateLTrimmed $ parseJValue afterColon
                    rest' = if head rest == ',' then tail rest else rest
                    acc' = (key, val) : acc
                in parseJObject acc' rest'
        parseJObjectValue _                  = undefined
    in parseJObjectValue restAfterKey

parseJObject _ _                = undefined


parseJString :: String  -> String -> (String, String)
parseJString acc ('\"' : xs) = (reverse acc, xs)
parseJString acc (x : xs) = parseJString (x : acc) xs

main :: IO()
main = print (parseJValue "{ \"id\" : 5, \"inner\" : {\"aba\" : false }, \"value\" : \"\" }")
-- main = print (JObject [("foo", JNumber 1), ("bar", JBool False)])   