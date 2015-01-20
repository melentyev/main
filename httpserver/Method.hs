module Method (Method(..)) where

data Method = Get 
			| Post
			| Put
			| Delete	
			deriving (Show)