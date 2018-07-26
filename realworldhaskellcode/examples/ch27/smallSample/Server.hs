 -- Echo server program
 module Main where

 import Control.Monad
 import qualified Data.ByteString as S
 import Network.Socket hiding (recv)
 import Network.Socket.ByteString

 main :: IO ()
 main = withSocketsDo $
     do addrinfos <- getAddrInfo Nothing (Just "") (Just "3000")
        let serveraddr = head addrinfos
        sock <- socket (addrFamily serveraddr) Stream defaultProtocol
        bindSocket sock (addrAddress serveraddr)
        listen sock 1
        (conn, _) <- accept sock
        talk conn
        sClose conn
        sClose sock

     where
       talk :: Socket -> IO ()
       talk conn =
           do msg <- recv conn 1024
              print msg
              unless (S.null msg) $ sendAll conn (S.reverse msg) >> talk conn


