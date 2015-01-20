module Service (Service(..), start) where

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as BS
import System.IO
import Control.Concurrent
import Data.Maybe
import qualified Data.Map as Map

import qualified Resource as Res
import qualified Request as Req
import qualified Response as Resp

data Service = Service {
    port :: Int,    
    resources :: [Res.Resource] 
} deriving (Show)

data StateTag = ParseRequestLine
              | ParseHeader
              | ParseBody
              | Parsed
              | ParseFailed

data StateData = StateData {
    tag           :: StateTag,
    url           :: String,
    contentLength :: Int,
    request       :: Request,
    buffer        :: String,
    isBadRequest  :: Bool
} deriving (Show)

initialState :: StateData
initialState = StateData { 
    tag           = RequestLine, 
    url           = Nothing, 
    contentLength = 0, 
    request       = Req.Request { 
        Req.path        = 0, 
        Req.method      = Method.Get, 
        Req.body        = BS.pack "", 
        Req.headers     = Map.empty, 
        Req.queryParams = Map.empty 
    },
    buffer        = BS.pack ""
    isBadRequest  = False
}

serviceLoop :: Socket -> Service -> IO ()
serviceLoop sock service = do
    conn <- accept sock
    forkIO (clientThread conn initialState service)
    serviceLoop sock service

findResource :: Service -> Request -> Maybe Resource
findResource s req = find (\r -> Res.path s == Req.path req && Res.method s == Req.method r) $ resources service

clientThread :: (Socket, SockAddr) -> StateData -> Service -> IO ()
clientThread (sock, sa) state service = do
    buf <- recv sock 1024
    if B8.null buf 
    then sClose socket
    else 
        let state' = handleData state buf in
        case tag state' of 
            ParseFailed -> sClose socket
            Parsed     ->
                let request = Req.request state' in
                case findResource service request of 
                    Just resource -> handleRequest resource request
                    Nothing -> Req.badRequest
                
            _ -> clientThread (sock, sa) state'

httpLineEnding = BS.pack "\r\n"

handleData :: StateData -> String -> StateData 
handleData st buf =
    hande (st {buffer = BS.append (buffer state) buf }) $ tag state
    where
        handle state@StateData { tag = ParseRequestLine } = 
            let (pref, suf) = breakSubstring httpLineEnding nbuf in  
            if BS.null suf then state
            else 
                case parseRequestLine pref of
                    Just (method, url, httpVersion) -> 
                        let req' = (Req.request state) { Req.method = method, Req.url = url, Req.httpVersion = httpVersion } in
                        handle $ state { tag = ParseHeader, request = req', buffer = drop 2 suf }
                    Nothing -> state { tag = ParseFailed } 
        handle state@StateData { tag = ParseHeader } = 
            let (pref, suf) = breakSubstring httpLineEnding nbuf in  
            if BS.null suf then state
            else
                case parseHeader pref of
                    Just (key, val, isLastHeader) -> 
                        let req' = (Req.request state) { Req.method = method, Req.url = url, Req.httpVersion = httpVersion } in
                        let tag' = if not isLastHeader then ParseHeader else ParseBody in
                        let contLen' = if key == "Content-Length" then read val else 0                   
                        handle $ state { tag = tag', request = req', buffer = drop 2 suf, contentLength = contLen' } 
        handle state@StateData { tag = ParseBody } = 
            let contLen = contentLength state
            let bufLen = BS.length $ buffer state
            let cpyLen = min bufLen (contLen - BS.length $ Req.body)
            let nbuf = BS.drop cpyLen $ buffer state
            if  contLen == cpyLen then 
            if 


start :: Service -> IO ()
start service = withSocketsDo $ do
    putStrLn "Service started"
    sock <- (socket AF_INET Stream 0)
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet (fromIntegral $ port service) iNADDR_ANY)
    listen sock 20
    serviceLoop sock service




    putStrLn "Service started" >> (socket AF_INET Stream 0 >>= (\sock -> setSocketOption sock ReuseAddr 1))
