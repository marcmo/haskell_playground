module Main where

import Control.Concurrent
import Network
import Network.Socket as S
import System.IO
import System.Timeout
import Control.Monad
import qualified Data.ByteString as S

main :: IO ()
main = do
    s <- listenOn (Service "7000")
    m <- newEmptyMVar
    forkIO $ receive s m
    send
    takeMVar m
    return ()

send :: IO ()
send = do
    hdl <- connectTo "localhost" (Service "7000")
    hSetBuffering hdl LineBuffering
    threadDelay 5000
    forM_ ["1","2","FINISH"] $ \str -> do
      hPutStrLn hdl str
      resp <- hGetLine hdl
      putStrLn $ "Sent " ++ str ++ ", got " ++ resp
    threadDelay 6000000
    hClose hdl

receive :: Socket -> MVar (S.ByteString) -> IO ()
receive s = do
    (s',_) <- S.accept s
    forkIO $ echo s'
    receive s
    
echo :: Socket -> IO ()
echo s = do
    mstr <- timeout 5000000 $ recv s 1000
    case mstr of
        Just str -> do
            send s str
            echo s
        Nothing -> do
            putStrLn "Time out"
            sClose s
            return ()

