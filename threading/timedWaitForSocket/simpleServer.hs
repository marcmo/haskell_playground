-- Echo server program
module Main where

import Control.Monad (unless,when)
import Network.Socket hiding (recv)
import qualified Data.ByteString as S
import Data.Word(Word8)
import Control.Concurrent(threadDelay)
import Network.Socket.ByteString (recv, sendAll)
import Data.Serialize
import SimpleMessage
import Numeric
import Data.List

main :: IO ()
main = withSocketsDo $
    do addrinfos <- getAddrInfo
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just "3333")
       let serveraddr = head addrinfos
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol
       bindSocket sock (addrAddress serveraddr)
       listen sock 1
       loop sock

    where
      loop s = do
        (connSock, _) <- accept s
        talk connSock
        loop s
        -- sClose connSock
        -- sClose sock

      talk :: Socket -> IO ()
      talk connSock = do 
             putStrLn "now we are talking..."
             debugSock connSock
             msg <- recv connSock 1024
             unless (S.null msg) $ do
              print $ "received over the wire: " ++ (showBinString msg)
              threadDelay(1000*1000)
              either
                (const $ print "could not parse message")
                ((sendAll connSock) . replyTo)
                (runGet get msg)
              putStrLn "sent back response, starting to listen again..."
              talk connSock
      replyTo :: SimpleMessage -> S.ByteString
      replyTo m = (runPut . put) response
        where responsePayload
                | [0xA] `isPrefixOf` (payload m) = [0x62,0x20,0x0]
                | [0xB] `isPrefixOf` (payload m) = [0x12,0x10,0x0]
                | otherwise = (head (payload m) + 0x40):(tail (payload m))
              response = SimpleMessage  (length responsePayload) responsePayload


debugSock s = do
    sIsConnected s >>= condPrint "connected!"
    sIsBound s >>= condPrint "bound"
    sIsListening s >>= condPrint "listening"
    sIsReadable s >>= condPrint "readable"
    sIsWritable s >>= condPrint "writable!"

condPrint s b = if b then putStrLn ("is " ++ s) else putStrLn ("NOT " ++ s)

