import Data.Bits
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString
import Data.List(intercalate)
import Data.List.Split(chunksOf)
import Numeric (showHex)
import Control.Monad(when)

prettyPrint :: BS.ByteString -> String
prettyPrint = unlines . map (intercalate ",") . chunksOf 10 . map (\x-> "0x" ++ showHex x "") . BS.unpack

type HandlerFunc = SockAddr -> BS.ByteString -> IO ()

serveLog :: String              -- ^ Port number or name; 514 is default
         -> HandlerFunc         -- ^ Function to handle incoming messages
         -> IO ()
serveLog port handlerfunc = withSocketsDo $
    do -- Look up the port.  Either raises an exception or returns
       -- a nonempty list.
       addrinfos <- getAddrInfo
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just port)
       let serveraddr = head addrinfos

       -- Create a socket
       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

       -- Bind it to the address we're listening to
       bindSocket sock (addrAddress serveraddr)

       -- Loop forever processing incoming data.  Ctrl-C to abort.
       procMessages sock
    where procMessages sock =
              do -- Receive one UDP packet, maximum length 1024 bytes,
                 -- and save its content into msg and its source
                 -- IP and port into addr
                 (msg, addr) <- recvFrom sock 1024
                 -- Handle it
                 handlerfunc addr msg
                 -- And process more messages
                 procMessages sock

-- A simple handler that prints incoming packets
plainHandler :: HandlerFunc
plainHandler (SockAddrInet remotePort addr) msg = do
    when (msg == ecu_mode_request) $ print "was ecu_mode_request"
    let payload = if msg == ecu_mode_request
                    then ecu_mode_response_application
                    else C.pack "yes"
    remoteIp <- inet_ntoa addr
    print $ "From " ++ remoteIp ++ "(remotePort:" ++ show remotePort ++ "):" ++ prettyPrint msg
    -- let port = 69
    addrinfos <- getAddrInfo Nothing (Just remoteIp) (Just $ show remotePort)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    let target = SockAddrInet remotePort addr
    print $ "sending to " ++ show target ++ " (payload:" ++ prettyPrint payload ++ ")"
    sent <- sendTo sock payload target
    print sent
plainHandler x _ = error $ "unexpected: " ++ show x

main = serveLog "5000" plainHandler


ecu_mode_request = BS.pack [0x30, 0x39, 0x00, 0x02, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x00, 0x01, 0x01, 0x00, 0x00]
ecu_mode_response_application = BS.pack [0x30, 0x39, 0x00, 0x02, 0x00, 0x00, 0x00, 0x09, 0x00, 0x00, 0x00, 0x00, 0x01, 0x01, 0x80, 0x00, 0x02]
ecu_mode_response_bootloader = BS.pack [0x30, 0x39, 0x00, 0x02, 0x00, 0x00, 0x00, 0x09, 0x00, 0x00, 0x00, 0x00, 0x01, 0x01, 0x80, 0x00, 0x01]
run_bootloader_request = BS.pack [0x30, 0x39, 0x00, 0x01, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x00, 0x01, 0x01, 0x01, 0x00]

