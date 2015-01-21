module Service (Service(..), start) where

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as BS
import System.IO
import Control.Concurrent
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.List as List

import qualified Resource as Res
import qualified Request as Req
import qualified Response as Resp
import qualified Method as Method
import qualified Parser as P

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
    request       :: Req.Request,
    buffer        :: String,
    isBadRequest  :: Bool,
    bodyRecved    :: Int,
    body          :: BS.ByteString,
    bodyReader    :: Res.BodyStream
} deriving (Show)

initialState :: StateData
initialState = StateData { 
    tag           = ParseRequestLine, 
    url           = Nothing, 
    contentLength = 0, 
    request       = Req.Request { 
        Req.path        = 0, 
        Req.method      = Method.Get,     
        Req.headers     = Map.empty, 
        Req.queryParams = Map.empty 
    },
    buffer        = BS.pack "",
    isBadRequest  = False,
    bodyRecved    = 0,
    body          = BS.pack ""
}

serviceLoop :: Socket -> Service -> IO ()
serviceLoop sock service = do
    conn <- accept sock
    forkIO (clientThread conn initialState service)
    serviceLoop sock service

findResource :: Service -> Req.Request -> Maybe Resource
findResource s req = List.find (\r -> Res.path s == Req.path req && Res.method s == Req.method r) $ resources service


httpLineEnding = BS.pack "\r\n"


clientThread :: (Socket, SockAddr) -> StateData -> Service -> IO ()
clientThread (sock, sa) clientState service = do
    buf <- recv sock 1024
    if B8.null buf 
    then sClose socket
    else 
        let state' = handleData clientState buf in
        case tag state' of 
            ParseFailed -> sClose socket
            Finished    -> sClose socket
            Parsed      ->
                let request = Req.request state' in
                case findResource service request of 
                    Just resource -> handleRequest resource request (bodyReader state')
                    Nothing -> Req.badRequest
            _ -> clientThread (sock, sa) state'                  

handleData :: StateData -> ByteString -> Service -> StateData 
handleData st buf service =
    handle $ st { buffer = BS.append (buffer st) buf }
    where
        handle state@StateData { tag = ParseRequestLine } = 
            let (pref, suf) = BS.breakSubstring httpLineEnding (buffer state) in  
            if BS.null suf then state
            else 
                case P.parseRequestLine pref of
                    Just (method, path, queryParams, httpVersion) -> 
                        handle $ state { tag = ParseHeader, request = req', buffer = drop 2 suf }
                        where req' = (request state) { 
                                Req.method = method, 
                                Req.path = path,
                                Req.queryParams = queryParams,
                                Req.httpVersion = httpVersion 
                            } 
                    _ -> state { tag = ParseFailed } 
        handle state@StateData { tag = ParseHeader } = 
            let (pref, suf) = BS.breakSubstring httpLineEnding (buffer state) in  
            if BS.null suf then state
            else
                case P.parseHeader pref of
                    Just (key, val, isLastHeader) -> 
                        handle $ state { 
                            tag = if not isLastHeader then ParseHeader else ParseBody, 
                            request = req', 
                            buffer = drop 2 suf, 
                            contentLength = contLen'
                        }
                        where
                            req' = (request state) { Req.headers = headers' }
                            contLen' = if key == "Content-Length" then read val else 0                   
                            headers' = (if not isLastHeader then Map.insert key val else id) $ (Req.headers state)
                    _ -> state { tag = ParseFailed }
        handle state@StateData { tag = ParseBody } = 
            if newLen == contLen 
            then state' { tag = Parsed, bodyReader = reader' 0 } 
            else handle state' 
            where
                request         = request state'
                res             = findResource service request
                reader' off len = 
                    if off >= newLen 
                    then Res.EOF 
                    else Res.Success (BS.take rdLen $ BS.drop off $ body state', reader' $ off + len)
                    where rdLen = min len (newLen - off)

                contLen = contentLength state
                cpyLen = min (BS.length $ buffer state) (contLen - bodyRecved state)        
                state' = state {
                    body = BS.append (body state) (BS.take cpyLen $ buffer state),
                    buffer = BS.drop cpyLen $ buffer state,
                    bodyRecved = bodyRecved state + cpyLen
                }
                newLen = (BS.length . body) state'
        --handle state@StateData { tag = Parsed }    =
        --        state { tag = Finished }
            {-let request = Req.request state' in
            case findResource service request of 
                    Just resource -> 
                        handleRequest resource request reader
                        where 
                            bufferReader \n = let cpyLen = min n (contentLength state)     
                            reader = if BS.null $ buffer state then sockReader else bufferReader
                            let contLen = contentLength state
                            let bufLen = BS.length $ buffer state
                            let cpyLen = min bufLen (contLen - BS.length $ Req.body)
                            reader = return $ Right $ BS.take cpyLen buffer state, internalReader 
                            internalReader = do
                                buf <- recv sock 1024
                                if B8.null buf 
                                then sClose socket >> return $ Left False
                                else 


                    Nothing -> Resp.notFound
            let nbuf = BS.drop cpyLen $ buffer state
            let nbody = BS.append
            if  contLen == cpyLen then -- handled
            if -}


start :: Service -> IO ()
start service = withSocketsDo $ do
    putStrLn "Service started"
    sock <- (socket AF_INET Stream 0)
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet (fromIntegral $ port service) iNADDR_ANY)
    listen sock 20
    serviceLoop sock service




    putStrLn "Service started" >> (socket AF_INET Stream 0 >>= (\sock -> setSocketOption sock ReuseAddr 1))
