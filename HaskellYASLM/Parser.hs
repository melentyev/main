module Parser where

import Data.Char
import Data.List
import qualified Data.ByteString.Lazy as BSL
import Control.Monad
import Control.Monad.Trans.State

data ParsingStateData = ParsingStateData { 
    psdStringRest  :: BSL.ByteString,
    psdLogged      :: [String]
} deriving (Show)

type ParsingState a = State ParsingStateData a


main :: IO()
main = 
    print "aaa"