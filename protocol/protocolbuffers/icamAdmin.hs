{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad
import Text.ProtocolBuffers(messageGet,messagePut,Utf8(..),defaultValue)
import Text.ProtocolBuffers.Header(Utf8(..))
import Text.ProtocolBuffers.Basic(utf8)
import qualified Data.ByteString as BS
import System
import Data.Char
import qualified Data.Foldable as F
import Data.Maybe(fromMaybe)
import qualified Data.ByteString.Lazy.UTF8 as U(fromString)
import qualified System.IO.UTF8 as U(getLine,putStr)
import Data.Sequence((|>),empty)
import Network(PortID(PortNumber),connectTo)
import Network.Socket
import Network.Socket.ByteString as SBS
import System.IO
import Control.Concurrent(putMVar,MVar,forkIO,newEmptyMVar,takeMVar)
import Foreign(Ptr,Word8,free,mallocBytes)
import Foreign.C.String(peekCStringLen)
import Foreign.C.Types(CChar)
import Control.Monad.Reader
import Control.Exception
import Debug.Trace
import Util.Encoding
import Text.Printf(printf)
import Prelude hiding (catch,log)

-- import Service.ServiceId
-- import Service.IcamManagement
-- import Service.AckVoid


-- main = do
--   print "hi"
  -- args <- getArgs
  -- file <- case args of
  --           [file] -> return file
  --           _ -> getProgName >>= \self -> error $ "Usage "++self++" ADDRESS_BOOK_FILE"
  -- f <- L.readFile file
  -- newBook <- case messageGet f of
  --              Left msg -> error ("Failed to parse address book.\n"++msg)
  --              Right (address_book,_) -> do
  --               putStrLn "got it..." 
  --               F.mapM_ (\p -> putStrLn $ show p) (person address_book)
  --               promptForAddress address_book
  -- seq newBook $ L.writeFile file (messagePut newBook)


-- void UdpRpcHandler::prepareRpcMessageHeader(
-- ...
-- 	writeMem32(&txBuffer[OFFSET_TYPE], type);
-- 	writeMem32(&txBuffer[OFFSET_SESSION_ID], sessionId);
-- 	writeMem32(&txBuffer[OFFSET_SERVICE_ID], serviceId);
-- 	writeMem32(&txBuffer[OFFSET_INSTANCE_ID], instanceId);
-- 	writeMem32(&txBuffer[OFFSET_METHOD_ID], methodId);

-- void UdpRpcHandler::dataReceived(
-- 	::ip::IPAddress sourceAddress,
-- 	uint16 sourcePort,
-- 	uint16 length)
-- {
-- 	Logger::log(_RPC, _DEBUG, "received %d byte of data", length);
-- 	assert(length < fRxBufferSize);
-- 	fSocket.read(fRxBuffer, length);
-- 	uint32 rxLength = readMem32(&fRxBuffer[OFFSET_LENGTH]);
-- 	assert(length >= rxLength);
-- 	uint32 type = readMem32(&fRxBuffer[OFFSET_TYPE]);
-- 	uint32 sessionId = readMem32(&fRxBuffer[OFFSET_SESSION_ID]);
-- 	uint32 serviceId = readMem32(&fRxBuffer[OFFSET_SERVICE_ID]);
-- 	uint32 instanceId = readMem32(&fRxBuffer[OFFSET_INSTANCE_ID]);
-- uint32 methodId = readMem32(&fRxBuffer[OFFSET_METHOD_ID]);


toWord :: (Enum a) => a -> Word8
toWord = int2Word8 . fromEnum
data RpcHeaderType = RPC_TYPE_REQUEST
                        | RPC_TYPE_RESPONSE
                        | RPC_TYPE_ERROR
                           deriving (Eq, Ord, Show, Read, Enum)
bootloaderMessage =
  let typeInfo = encodeInt (toWord RPC_TYPE_REQUEST) 4
      sessionId = encodeInt (toWord 0) 4
      serviceId = encodeInt (toWord 50) 4
      instanceId = encodeInt (toWord 1) 4
      methodId = encodeInt (toWord 0) 4
  in
  BS.pack $ concat [typeInfo,sessionId,serviceId,instanceId,methodId]

main = do
  let c = MkDiagConfig { host="160.48.199.9", port=30456, verbose=True }
  sendMessage c bootloaderMessage

data DiagConnection = DiagConnection { diagSocket :: Socket, addr :: SockAddr, chatty :: Bool }
data DiagConfig = MkDiagConfig {
  host :: String,
  port :: Int,
  verbose :: Bool
} deriving (Show)

type Net = ReaderT DiagConnection IO
sendMessage :: DiagConfig -> BS.ByteString -> IO ()
-- sendMessage c msg = runReaderT (run msg) 
-- sendMessage c msg = bracket (diagConnect c) disconnect loop
sendMessage c msg = do
    con <- diagConnect c
    loop con
  where
    loop st    = catch (runReaderT (run msg) st) (\(_ :: IOException) -> print "io-exception happened!")

diagConnect :: DiagConfig -> IO DiagConnection
diagConnect c = notify $ do
    -- Look up the hostname and port.  Either raises an exception
    -- or returns a nonempty list.  First element in that list
    -- is supposed to be the best option.
    addrinfos <- getAddrInfo Nothing (Just $ host c) (Just $ show $ port c)
    let serveraddr = head addrinfos
    -- Establish a socket for communication
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    -- Save off the socket, program name, and server address in a handle
    return $ DiagConnection sock (addrAddress serveraddr) (verbose c)
    -- hSetBuffering h NoBuffering
  where
    notify
      | verbose c = bracket_ (printf "Connecting to %s ... " (host c) >> hFlush stdout) (putStrLn "done.")
      | otherwise = bracket_ (return ()) (return ())

run :: BS.ByteString -> Net ()
run msg = pushOutMessage msg

pushOutMessage :: BS.ByteString -> Net ()
pushOutMessage msg = do
    h <- asks diagSocket
    sockAddr <- asks addr
    log ("--> " ++ show msg)
    io $ print $ "sending over the wire: " ++ (show msg)
    io $ SBS.sendAllTo h msg sockAddr

liftReader a = ReaderT (return . runReader a)

-- Convenience.
io :: IO a -> Net a
io = liftIO
log ::  (Show a) => a -> Net ()
log s = do
    v <- asks chatty
    when v $ io $ print s
