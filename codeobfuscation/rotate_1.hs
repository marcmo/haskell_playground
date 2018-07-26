import Data.Char
import System.Environment
import System.IO
import System.Directory
import Control.Monad
import qualified Data.Set as S
import Debug.Trace
 
rot13 :: Char -> Char
rot13 c
  | toLower c >= 'a' && toLower c <= 'm' = chr (ord c + 13)
  | toLower c >= 'n' && toLower c <= 'z' = chr (ord c - 13)
  | otherwise = c

hInteract :: (String -> String) -> Handle -> Handle -> IO ()
hInteract f hIn hOut = do
  inLines <- lines `fmap` hGetContents hIn
  let outLines = map f inLines
  hPutStr hOut (concat outLines)
 
processByTemp :: (Handle -> Handle -> IO ()) -> String -> IO ()
processByTemp f name = do
  hIn <- openFile name ReadMode
  let tmp = name ++ "$" 
  hOut <- openFile tmp WriteMode
  f hIn hOut
  hClose hIn
  hClose hOut
  -- removeFile name
  -- renameFile tmp name
 
process :: (Handle -> Handle -> IO ()) -> [String] -> IO ()
process f [] = f stdin stdout
process f ns = mapM_ (processByTemp f) ns

main = do
 names <- getArgs
 putStrLn $ "names:" ++ show names
 process (hInteract rot13words) names

rot13words :: String -> String
rot13words x = (trace $ "words: " ++ show ws) rotatedWs
      where ws = words x
            rotW w = if w `S.member` reservedWords then w else (map rot13 w)
            rotatedWs = concatMap rotW ws 

reservedWords = S.fromList ["import","::","++"] 

