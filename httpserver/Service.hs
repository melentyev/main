module Service (Service(..), start) where

import Network.Socket
import System.IO
import qualified Resource as Res
import Control.Concurrent
import Data.Maybe
import qualified Data.Map as Map

data Service = Service {
    port :: Int,    
    resources :: [Res.Resource] 
} deriving (Show)

data StateTag = Started
              |

data StateData = StateData {
    tag :: StateTag,
    url :: Maybe String,
    contentLength :: Int,
    headers :: Map String String,
    body :: String,
    buffer :: String,
}

initialState = StateData { 
    tag = Started, 
    url = Nothing, 
    contentLength = 0, 
    headers = Map.empty, 
    body = [], buffer = [] 
}

serviceLoop :: Socket -> IO ()
serviceLoop sock = do
    conn <- accept sock
    forkIO (clientThread conn initialState)
    serviceLoop sock

clientThread :: (Socket, SockAddr) -> IO ()
clientThread (sock, _) state = do

    clientThread state'

start :: Service -> IO ()
start service = do
    putStrLn "Service started"
    sock <- (socket AF_INET Stream 0)
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet (fromIntegral $ port service) iNADDR_ANY)
    listen sock 20
    serviceLoop sock