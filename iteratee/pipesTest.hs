import Control.Proxy
import Network
import System.IO
import Control.Concurrent
import Control.Monad

name :: Proxy p => () -> Producer p String IO ()
name () = runIdentityP $ do
  lift $ putStr "Ho ho ho! What is your name? "
  lift getLine >>= respond

data Present = Present String
  deriving (Show)

presents :: Proxy p => () -> Producer p Present IO ()
presents () = runIdentityP $
    lift (putStrLn "And what presents would you like?") >> go ()
  where
    go = getLineS >-> takeWhileD (not . null) >-> mapD Present

server ::  IO ()
server = withSocketsDo $ do
    s <- listenOn (PortNumber 5553)
    (h, _, _) <- accept s
    hSetBuffering h LineBuffering
    runProxy $ hGetLineS h >-> putStrLnD
    hClose h

client ::  IO ()
client = withSocketsDo $ do
    h <- connectTo "localhost" (PortNumber 5553)
    hSetBuffering h LineBuffering
    runProxy $ getLineS >-> takeWhileD (/= "quit") >-> hPutStrLnD h
    hClose h

test = forkIO server >> client

chars' ::  Proxy p => () -> Producer p Char IO ()
chars' () = runIdentityP loop where
    loop = do
        eof <- lift isEOF
        unless eof $ do
            str <- lift getChar
            _ <- respond str  -- Produce the string
            loop

printer :: (Proxy p, Show a) => () -> Consumer p a IO r
printer () = runIdentityP $ forever $ do
    a <- request ()  -- Consume a value
    lift $ putStr "Received a value:"
    lift $ print a

threeReqs :: (Proxy p) => () -> Client p Int Char IO ()
threeReqs () = runIdentityP $ forM_ [1, 3, 1] $ \argument -> do
    result <- request argument
    lift $ putStrLn $ "Client Receives:" ++ show result
    lift $ putStrLn "*"

promptInt :: (Proxy p) => () -> Producer p Int IO r
promptInt () = runIdentityP $ forever $ do
    lift $ putStrLn "Enter an Integer:"
    n <- lift readLn  -- 'lift' invokes an action in the base monad
    respond n

main = runProxy $ chars' >-> printer

consumeString :: String -> (Maybe Char, Maybe Char)
consumeString = foldl (flip consumeChar) (Nothing,Nothing)

consumeChar :: Char -> (Maybe Char,Maybe Char) -> (Maybe Char,Maybe Char)
consumeChar c (Nothing,Nothing) = (Just c,Nothing)
consumeChar c p@(Just d,Nothing)
  | c == d = p
  | otherwise = (Just c, Just d)
consumeChar c p@(Just d,Just e)
  | c == d = p
  | c == e = (Just e, Just d)
  | otherwise = (Just c, Just d)

