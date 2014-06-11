module Main where

import Data.Char
import Data.List
import Control.Monad
import Control.Monad.Trans.State
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

type StateLog a = State [String] a
data ParsingState = ParsingState { 
    psStringRest  :: String,
    psLogged      :: [String]
} deriving (Show)
type StateHolder a = State ParsingState a

ltrim :: String -> String
ltrim = dropWhile (== ' ') 

rtrim :: String -> String
rtrim = dropWhileEnd (== ' ') 

stateLTrimmed :: (a, String) -> (a, String)
stateLTrimmed (a, xs) = (a, ltrim xs)

trim :: String -> String
trim = ltrim . rtrim

record :: String -> StateHolder ()
record s = modify (\st -> st { psLogged = (psLogged st) ++ [s] } )
consume :: Int -> StateHolder ()
consume n = modify (\st -> st { psStringRest = drop n (psStringRest st) })
consumeSpaces ::  StateHolder ()
consumeSpaces = modify (\st -> st { psStringRest = ltrim (psStringRest st) })
rest :: String -> StateHolder ()
rest s = modify (\st -> st { psStringRest = s } )
--record s = modify (++[s])

parseJValue :: StateHolder JValue
parseJValue = do
    st0 <- get
    case psStringRest st0 of
        (' ' : xs) -> do { rest xs; parseJValue }
        ('{' : xs) -> do { rest xs; JObject `liftM` (parseJObject []) }
        ('\"' : xs) -> do { rest xs; JString `liftM` (parseJString "") }
        (xs)        ->
            if ("true" `isPrefixOf` xs) then do 
                    record "found 'true'"
                    consume 4
                    return $ JBool True
            else if ("false" `isPrefixOf` xs) then do 
                    record "found 'false'"
                    consume 5
                    return $ JBool False
            else do
                    let isNumberPart c = 
                            c /= '.' && not (isDigit c)
                    let    (val, xs') = break isNumberPart xs
                    let    (val', _) = head $ (reads val :: [(Double, String)])     
                    rest xs'
                    return $ JNumber val'

parseJObject :: [(String, JValue)] -> StateHolder [(String, JValue)]
parseJObject acc = do
    st0 <- get
    case psStringRest st0 of
        (' ' : xs)     -> do { rest xs; parseJObject acc }
        ('}' : xs)     -> do { rest xs; return $ reverse acc }
        ('\"' : xs)    -> do
            rest xs
            key <- parseJString ""
            consumeSpaces
            st1 <- get
            case psStringRest st1 of
                (':' : afterColon)  -> do
                    rest afterColon
                    val <- parseJValue
                    consumeSpaces
                    st2 <- get
                    let finalRest = case psStringRest st2 of
                                        (',' : rest'') -> rest''
                                        rest'' -> rest''
                    rest finalRest
                    let acc' = (key, val) : acc
                    parseJObject acc'
                _                   -> error "error finalRest = case psStringRest st1 of (in parseJObject)"
        rst              -> error ("error parseJObject" ++ show rst)


parseJString :: String -> StateHolder String
parseJString acc = do
    st0 <- get
    case psStringRest st0 of
        ('\"' : xs) -> do
            record "found String"
            rest xs
            return $ reverse acc
        (x : xs)    -> do { rest xs; parseJString (x : acc) }

runParser :: String -> (JValue, ParsingState)
runParser s = 
    let 
        (a, b) = runState parseJValue $ ParsingState s []
    in (a, b)

main :: IO()
--main = print (runParser "{ \"id\" : true }")
main = print (runParser "{ \"id\" : true, \"inner\" : {\"aba\" : false }, \"value\" : \"\" }")
-- main = print (JObject [("foo", JNumber 1), ("bar", JBool False)])   