module Method (Method(..), fromString) where

import Data.Char

data Method = Get 
			| Post
			| Put
			| Delete	
			deriving (Eq, Show)

fromString :: String -> Method
fromString s = 
	case map toUpper s of 
		"GET"    -> Get
		"POST"   -> Post
		"PUT"    -> Put
		"DELETE" -> Delete
	