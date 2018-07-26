{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import SimpleMessage
import Network(PortID(PortNumber),connectTo)
import System.IO hiding (hPutStrLn,hPutStr)
import Data.ByteString.Char8 hiding (putStrLn,putStr)
import qualified Data.ByteString as S
import Control.Concurrent(putMVar,MVar,forkIO,newEmptyMVar,takeMVar,yield)
import Foreign(Ptr,Word8,free,mallocBytes)
import Foreign.C.String(peekCStringLen)
import Foreign.C.Types(CChar)
import Control.Exception
import Text.Printf(printf)
import Debug.Trace
import Prelude hiding (catch,log)

data DiagConfig = MkDiagConfig {
  host :: String,
  port :: Int,
  source :: Word8,
  target :: Word8,
  verbose :: Bool,
  diagTimeout :: Int
} deriving (Show)
data DiagConnection = MkDiagConnection {
	diagHandle :: Handle,
	connectionTimeout :: Int
}
timeout = 1000 :: Int

sendBytes :: DiagConfig -> Handle -> [Word8] -> IO (Maybe SimpleMessage)
sendBytes c h = sendMessage c h . dataMessage

sendMessage :: DiagConfig -> Handle -> SimpleMessage -> IO (Maybe SimpleMessage)
sendMessage c h msg = bracket connect disconnect loop
  where
    disconnect = hClose h
    loop st    = catch (run msg h) (\(_ :: IOException) -> return Nothing)
    connect = do
        h <- connectTo (host c) (PortNumber $ fromIntegral (port c))
        hSetBuffering h NoBuffering
        return (MkDiagConnection h (diagTimeout c))

