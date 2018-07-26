{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import SimpleMessage
import System.IO hiding (hPutStrLn,hPutStr)
import Data.ByteString.Char8 hiding (putStrLn,putStr)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Control.Concurrent(putMVar,MVar,forkIO,newEmptyMVar,yield,takeMVar)
import Network(PortID(PortNumber),connectTo)
import Foreign(Ptr,Word8,free,mallocBytes)
import Foreign.C.String()
import Foreign.C.Types(CChar)
import Control.Exception
import Text.Printf(printf)
import Debug.Trace
import Data.Serialize
import Prelude hiding (catch,log)

receiveBufSize = 4096 :: Int
timeout = 1000 :: Int
pollingMs = 100 :: Int

type Destination = (String,Int)

sendBytes :: Destination -> [Word8] -> IO (Maybe SimpleMessage)
sendBytes d = sendMessage d . dataMessage

sendMessage :: Destination -> SimpleMessage -> IO (Maybe SimpleMessage)
sendMessage (host,port) msg = bracket connect disconnect loop
  where
    disconnect = hClose
    loop h    = catch (sendAndReceive msg h) (\(_ :: IOException) -> return Nothing)
    connect = do
        h <- connectTo host (PortNumber $ fromIntegral port)
        hSetBuffering h NoBuffering
        return h

sendAndReceive :: SimpleMessage -> Handle -> IO (Maybe SimpleMessage)
sendAndReceive msg h = do
    m <- newEmptyMVar
    forkIO $ catch (listenForResponse h m)
              (\(e :: SomeException) -> print e >> putMVar m Nothing >> return ())
    pushOutMessage msg h
    takeMVar m 
  where
    pushOutMessage msg h = hPutStr h (runPut $ put msg) >> hFlush h 

listenForResponse ::  Handle -> MVar (Maybe SimpleMessage) -> IO ()
listenForResponse h m =
   do msg <- receiveResponse h
      putMVar m msg
      return ()
  where

    receiveResponse :: Handle -> IO (Maybe SimpleMessage)
    receiveResponse h = do
        buf <- mallocBytes receiveBufSize
        dataResp <- receiveMsg buf h
        free buf
        return dataResp

    receiveMsg :: Ptr CChar -> Handle -> IO (Maybe SimpleMessage)
    receiveMsg buf h = do
        dataAvailable <- waitForData timeout h
        if not dataAvailable then (print "no data available") >> return Nothing
          else do
            answereBytesRead <- hGetBufNonBlocking h buf receiveBufSize
            res <- S.packCStringLen (buf,answereBytesRead)
            return $ either (const Nothing) Just (runGet get res) 

    waitForData ::  Int -> Handle -> IO Bool
    waitForData waitTime_ms h = do
      S.putStr "->"
      yield
      hWaitForInput h waitTime_ms

    -- waitForData ::  Int -> Handle -> IO Bool
    -- waitForData waitTime_ms h = do
    --   S.putStr "."
    --   yield
    --   inputAvailable <- hWaitForInput h pollingMs
    --   if inputAvailable then return True 
    --     else if waitTime_ms > 0
    --           then waitForData (waitTime_ms - pollingMs) h
    --           else return False

