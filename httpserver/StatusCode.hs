module StatusCode (StatusCode(..), toInt) where

data StatusCode = Ok
				| BadRequest
				| NotFound
				deriving (Eq)

toInt :: StatusCode -> Int
toInt Ok         = 200
toInt BadRequest = 400
toInt NotFound   = 404

instance Show StatusCode where
	show = show . toInt