{-# LANGUAGE BangPatterns #-}
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy as L
import System.Environment
import Control.Concurrent
import Control.Monad (replicateM_)

main = do
    files <- getArgs
    str <- newEmptyMVar
    mapM_ (forkIO . hashAndPrint str) files
    printNrResults (length files) str

printNrResults i var = replicateM_ i (takeMVar var >>= putStrLn)

hashAndPrint str f = do
    bs <- L.readFile f
    let !h = show $ md5 bs
    putMVar str (f ++ ": " ++ h)
