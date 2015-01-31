{-# LANGUAGE FlexibleInstances #-}

module DataDecl where

import qualified Data.ByteString as BS
	
data StreamResult = EOF
    	          | Error
                  | Success (BS.ByteString, BodyStream)

type SendResponse = BS.ByteString -> IO ()

type BodyStream = Int -> IO StreamResult

instance Show (Int -> IO StreamResult) where
    show _ = "(-#BodyStream#-)"
