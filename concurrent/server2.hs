import Network
import Control.Monad
import Control.Concurrent
import System.IO
import Text.Printf
import Control.Concurrent.Async
import Control.Concurrent.STM

-- <<main
main = withSocketsDo $ do
  sock <- listenOn (PortNumber (fromIntegral port))
  printf "Listening on port %d\n" port
  factor <- atomically $ newTVar 2                               -- <1>
  clientCount <- atomically $ newTVar 0
  forever $ do
      (handle, host, _port) <- accept sock
      cc <- atomically $ modifyTVar clientCount (+1) >> readTVar clientCount
      printf "Accepted connection from %s: %s (%d clients)\n" host (show _port) cc
      forkFinally (talk handle factor clientCount) (\e -> do
        hClose handle
        c <- atomically $ modifyTVar clientCount (subtract 1) >> readTVar clientCount
        printf "client disconnected (%d clients left) (%s)\n" c (show e))

port :: Int
port = 44444
-- >>

-- <<talk
talk :: Handle -> TVar Integer -> TVar Int -> IO ()
talk h factor clientCount = do
  hSetBuffering h LineBuffering
  c <- atomically newTChan              -- <1>
  race_ (server h factor c clientCount) (receive h c)  -- <2>
  return ()
-- >>

-- <<receive
receive :: Handle -> TChan String -> IO ()
receive h c = forever $ do
  line <- hGetLine h
  atomically $ writeTChan c line
-- >>

-- <<server
server :: Handle -> TVar Integer -> TChan String -> TVar Int -> IO ()
server h factor c clientCount = do
  f <- atomically $ readTVar factor     -- <1>
  cc <- atomically $ readTVar clientCount
  hPrintf h "Current factor: %d (%d clients)\n" f cc    -- <2>
  loop f
 where
  loop f = do
    action <- atomically $ do           -- <4>
      f' <- readTVar factor             -- <5>
      if (f /= f')                      -- <6>
         then return (newfactor f')     -- <7>
         else do
           l <- readTChan c             -- <8>
           return (command f l)         -- <9>
    action

  newfactor f = do                      -- <10>
    hPrintf h "new factor: %d\n" f
    loop f

  command f s
   = case s of
      "end" -> do
        cc <- atomically $ do
          modifyTVar clientCount (subtract 1)
          readTVar clientCount
        hPrintf h ("Thank you for using the Haskell doubling service, %d clients left\n") cc
      '*':_s -> do
        atomically $ writeTVar factor (read _s :: Integer) -- <13>
        loop f
      line  -> do
        hPutStrLn h (show (f * (read line :: Integer)))
        loop f
-- >>

