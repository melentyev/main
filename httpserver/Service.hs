module Service (
    Service(..), 
    start, 
    service, 
    restService) where

import Network.Socket hiding (send, recv, sendAll)
import Network.Socket.ByteString (send, recv, sendAll)
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
import DataDecl

class Service a where
    port :: a -> Int
    requestHandler :: a -> Req.Request -> BodyStream -> SendResponse -> IO ()

data BasicService = BasicService {
    basicPort :: Int,
    basicRequestHandler :: Req.Request -> BodyStream -> SendResponse -> IO ()
}

instance Show BasicService where
    show a = "(BasicService {port=" ++ (show . port) a ++ "})"

data RestService = RestService {
    base :: BasicService,
    resources :: [Res.Resource]
} deriving (Show)

instance Service BasicService where
    port = basicPort
    requestHandler = basicRequestHandler

instance Service RestService where
    port = basicPort . base
    requestHandler = basicRequestHandler . base

service p rh = BasicService { 
    basicPort = p, 
    basicRequestHandler = rh
} 

restService p rs = s
    where s = RestService { 
        base = service p (restServiceRequestHandler s),
        resources = rs
    }

restServiceRequestHandler :: RestService -> Req.Request -> BodyStream -> SendResponse -> IO ()
restServiceRequestHandler service req br sendResp = do
    resp <- maybe 
        (return Resp.notFound) 
        (\resource -> Res.handleRequest resource req br )
        (findResource service req)
    print resp
    sendResp (Resp.rawData resp)

data StateTag = ParseRequestLine
              | ParseHeader
              | ParseBody
              | Parsed
              | ParseFailedRL
              | ParseFailedHS
              | Finished
              deriving (Show)

data StateData = StateData {
    tag           :: StateTag,
    contentLength :: Int,
    request       :: Req.Request,
    buffer        :: BS.ByteString,
    isBadRequest  :: Bool,
    bodyRecved    :: Int,
    body          :: BS.ByteString,
    bodyReader    :: Res.BodyStream
} deriving (Show)


nullstr = BS.empty

initialState :: StateData
initialState = StateData { 
    tag           = ParseRequestLine, 
    contentLength = 0, 
    request       = Req.Request { 
        Req.httpVersion = (1, 1),
        Req.path        = "", 
        Req.url         = "",
        Req.method      = Method.Get,     
        Req.headers     = Map.empty, 
        Req.queryParams = Map.empty 
    },
    buffer        = nullstr,
    isBadRequest  = False,
    bodyRecved    = 0,
    body          = nullstr,
    bodyReader    = \_ -> return Res.EOF
}

serviceLoop :: (Service a) => Socket -> a -> IO ()
serviceLoop sock service = do
    conn <- accept sock
    putStrLn "socket accepted"
    forkIO (clientThread conn initialState service)
    serviceLoop sock service

findResource :: RestService -> Req.Request -> Maybe Res.Resource
findResource s req = List.find (\r -> Res.path r == Req.path req && Res.method r == Req.method req) $ resources s

httpLineEnding = B8.pack "\r\n"

clientThread :: (Service a) => (Socket, SockAddr) -> StateData -> a -> IO ()
clientThread (sock, sa) clientState service = do
    buf <- recv sock 1024
    putStrLn $ "clientThread: (buf: " ++ show (BS.length buf) ++ ")"
    if B8.null buf 
    then sClose sock
    else do
        let state' = handleData clientState buf service 
        print state'
        (case tag state' of 
            ParseFailedRL -> sClose sock
            ParseFailedHS -> sClose sock
            Finished      -> sClose sock
            Parsed        -> do
                    let req = request state'
                    (requestHandler service) req (bodyReader state') (\bs -> sendAll sock bs)
                    sClose sock
            _ -> clientThread (sock, sa) state' service)

handleData :: (Service a) => StateData -> BS.ByteString -> a -> StateData 
handleData st buf service =
    handle $ st { buffer = BS.append (buffer st) buf }
    where
        handle state@StateData { tag = ParseRequestLine } = 
            let (pref, suf) = BS.breakSubstring httpLineEnding (buffer state) in  
            if BS.null suf then state
            else 
                case P.parseRequestLine pref of
                    Just (method, url, path, queryParams, httpVersion) -> 
                        handle $ state { tag = ParseHeader, request = req', buffer = BS.drop 2 suf }
                        where req' = (request state) { 
                                Req.method = method, 
                                Req.url = url,
                                Req.path = path,
                                Req.queryParams = queryParams,
                                Req.httpVersion = httpVersion 
                            } 
                    _ -> state { tag = ParseFailedRL } 
        handle state@StateData { tag = ParseHeader } = 
            let (pref, suf) = BS.breakSubstring httpLineEnding (buffer state) in  
            if BS.null suf then state
            else
                case P.parseHeader pref of
                    Just (key, val, isLastHeader) -> 
                        handle $ state { 
                            tag = if not isLastHeader then ParseHeader else ParseBody, 
                            request = req', 
                            buffer = BS.drop 2 suf, 
                            contentLength = contLen'
                        }
                        where
                            req' = (request state) { Req.headers = headers' }
                            contLen' = if key == "Content-Length" then read val else 0                   
                            headers' = (if not isLastHeader then Map.insert key val else id) $ (Req.headers . request) state
                    _ -> state { tag = ParseFailedHS }
        handle state@StateData { tag = ParseBody } = 
            if newLen == contLen 
            then state' { tag = Parsed, bodyReader = reader' 0 } 
            else handle state' 
            where
                req             = request state'
                --res             = findResource service request
                reader' off len = 
                    return $ if off >= newLen 
                    then Res.EOF 
                    else Res.Success (BS.take rdLen (BS.drop off $ body state'), reader' $ off + len)
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


start :: (Service a) => a -> IO ()
start service = withSocketsDo $ do
    putStrLn "Service started"
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet (fromIntegral $ port service) iNADDR_ANY)
    listen sock 20
    serviceLoop sock service




    putStrLn "Service started" >> (socket AF_INET Stream 0 >>= (\sock -> setSocketOption sock ReuseAddr 1))
