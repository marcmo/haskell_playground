module Main
    where

import qualified System.IO.UTF8 as U
import System.IO 

-- main = interact (map id)
      
main = do
  -- hSetBuffering stdin LineBuffering
  doLoop

doLoop = do
  -- U.putStrLn "Enter a command rFN wFN or q to quit:"
  -- command <- U.getLine
  putStrLn "hello"
  -- doLoop

-- doRead filename =
--     bracket (openFile filename ReadMode) hClose
--             (\h -> do contents <- hGetContents h
--                       putStrLn "The first 100 chars:"
--                       putStrLn (take 100 contents))
-- doWrite filename = do
--   putStrLn "Enter text to go into the file:"
--   contents <- getLine
--   bracket (openFile filename WriteMode) hClose
--           (\h -> hPutStrLn h contents)
-- 

