import Network.Transport
import Network.Transport.TCP (createTransport,defaultTCPParameters)
import Control.Concurrent
import Control.Monad
import Data.String

main :: IO ()
main = do
  serverAddr <- newEmptyMVar
  clientDone <- newEmptyMVar

  Right transport <- createTransport "127.0.0.1" "10080" defaultTCPParameters

  -- "Server"
  forkIO $ do
    Right endpoint <- newEndPoint transport
    putMVar serverAddr (address endpoint)

    forever $ do
      event <- receive endpoint
      case event of
        Received _ msg -> print msg
        _ -> return () -- ignore

  -- "Client"
  forkIO $ do
    Right endpoint <- newEndPoint transport
    Right conn     <- do addr <- readMVar serverAddr
                         connect endpoint addr ReliableOrdered defaultConnectHints
    send conn [fromString "Hello world"]
    putMVar clientDone ()

  -- Wait for the client to finish
  takeMVar clientDone
